"""
3a_solve_shocks.py

Solves for equilibrium responses to:
1. Severe domestic productivity shocks (25% per sector)
2. Severe foreign price shocks (25% per tradeable sector)
3. Calibrated domestic productivity shocks (1000 draws from estimated TFP distribution)

Saves results to clean_data/.
"""

# ============================================================================
# Toggle which shock types to run
# ============================================================================
RUN_SEVERE_DOMESTIC = True
RUN_SEVERE_FOREIGN = True
RUN_CALIBRATED = True

import sys
sys.path.insert(0, 'code')

import pandas as pd
import numpy as np
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from scipy.stats import multivariate_normal
from code.helper_fncts.soe_model import *

# ============================================================================
# Load calibration data
# ============================================================================
ro.r['load']('clean_data/calibration_data.RData')

with localconverter(ro.default_converter + pandas2ri.converter):
    Omega = ro.r['Omega_mat']
    Delta = ro.r['Delta_mat']
    calibration_data = ro.r['calibration_vectors']
    industry_tfp = ro.r['industry_TFP']

N = calibration_data.shape[0]
omega_mat = Omega.copy()
omega_mat[omega_mat < 1e-2] = 0
omega_mat = omega_mat / omega_mat.sum(axis=1, keepdims=True)

alpha_vec = calibration_data['alpha_it'].to_numpy()
beta_vec = calibration_data['beta_it'].to_numpy()

xi_mat = np.eye(N) - (omega_mat.T * (1 - alpha_vec))
xi_inv = np.linalg.solve(xi_mat, np.eye(N))
L_vec = alpha_vec * (xi_inv @ beta_vec)
calibration_data['L_it'] = L_vec

print(f"Sum of beta: {sum(beta_vec):.6f}")
print(f"Labor sum: {L_vec.sum():.6f}")


def make_params(config):
    """Create model parameters for a given configuration."""
    params = create_example_parameters(N=66)
    params.omega = omega_mat
    params.alpha = alpha_vec
    params.beta = beta_vec
    params.L = L_vec
    params.epsilon = 0.6
    params.sigma = calibration_data['sigma'].mean()
    params.gamma = calibration_data['gamma_uniform'].to_numpy()
    params.delta = config['delta'].copy()
    params.theta = config['theta'].copy()
    return params


# All configurations used across shock types
ALL_CONFIGS = {
    'cobbdouglas': {
        'delta': Delta,
        'theta': np.ones_like(calibration_data['theta_i'].to_numpy())
    },
    'cobbdouglas_closed': {
        'delta': np.ones_like(Delta),
        'theta': np.ones_like(calibration_data['theta_i'].to_numpy())
    },
    'main': {
        'delta': Delta,
        'theta': calibration_data['theta_i'].to_numpy()
    },
    'main_closed': {
        'delta': np.ones_like(Delta),
        'theta': calibration_data['theta_i'].to_numpy()
    },
    'common_theta': {
        'delta': Delta,
        'theta': calibration_data['theta_uniform'].to_numpy()
    },
    'common_theta_closed': {
        'delta': np.ones_like(Delta),
        'theta': calibration_data['theta_uniform'].to_numpy()
    }
}

# ============================================================================
# 1. Severe Domestic Productivity Shocks
# ============================================================================
if RUN_SEVERE_DOMESTIC:
    print("\n" + "="*60)
    print("SEVERE DOMESTIC PRODUCTIVITY SHOCKS")
    print("="*60)

    shock_size = 0.25
    severe_configs = ['main', 'main_closed', 'common_theta', 'common_theta_closed']
    severe_shocks_results = pd.DataFrame(index=range(N), columns=severe_configs)

    for config_name in severe_configs:
        print(f"\nConfiguration: {config_name}")
        params = make_params(ALL_CONFIGS[config_name])
        model = SOEModel(params, verbose=True)

        for sector_idx in range(N):
            params.Z[sector_idx] = 1 - shock_size
            solved = model.solve(verbose=False, method='minimize')
            severe_shocks_results.loc[sector_idx, config_name] = solved['C']
            print(f"  Sector {sector_idx + 1}/{N} shocked. C = {solved['C']:.6f}")
            params.Z[sector_idx] = 1.0

    severe_shocks_results.insert(0, 'Industry', calibration_data['Industry'].values)
    severe_shocks_results['main_minus_common'] = severe_shocks_results['main'] - severe_shocks_results['common_theta']
    severe_shocks_results['main_closed_minus_common_closed'] = severe_shocks_results['main_closed'] - severe_shocks_results['common_theta_closed']
    severe_shocks_results.sort_values(by='main_minus_common', inplace=True, ascending=False)
    severe_shocks_results.to_csv('clean_data/severe_shocks_results.csv', index=True)
    print("\nSevere shocks results saved.")

# ============================================================================
# 2. Severe Foreign Price Shocks
# ============================================================================
if RUN_SEVERE_FOREIGN:
    print("\n" + "="*60)
    print("SEVERE FOREIGN PRICE SHOCKS")
    print("="*60)

    tradeable_mask = np.any(Delta != 1, axis=0)
    tradeable_sectors = np.where(tradeable_mask)[0]
    print(f"Number of tradeable sectors: {len(tradeable_sectors)}")

    shock_size = 0.25
    foreign_price_shock_results = pd.DataFrame(
        index=tradeable_sectors,
        columns=['main', 'main_closed', 'common_theta', 'common_theta_closed']
    )
    sectoral_price_responses = {
        'main': pd.DataFrame(index=tradeable_sectors, columns=[f'sector_{i}' for i in range(N)]),
        'common_theta': pd.DataFrame(index=tradeable_sectors, columns=[f'sector_{i}' for i in range(N)])
    }

    for config_name in ['main', 'main_closed', 'common_theta', 'common_theta_closed']:
        print(f"\nConfiguration: {config_name}")
        params = make_params(ALL_CONFIGS[config_name])
        model = SOEModel(params, verbose=True)

        for sector_idx in tradeable_sectors:
            params.P_f[sector_idx] = 1 + shock_size
            solved = model.solve(verbose=False, method='minimize')
            foreign_price_shock_results.loc[sector_idx, config_name] = solved['C']
            if config_name in ['main', 'common_theta']:
                sectoral_price_responses[config_name].loc[sector_idx, :] = solved['P_d']
            print(f"  Sector {sector_idx + 1}/{N} ({calibration_data['Industry'].iloc[sector_idx][:30]}) shocked. C = {solved['C']:.6f}")
            params.P_f[sector_idx] = 1.0

    foreign_price_shock_results.insert(0, 'Industry', calibration_data['Industry'].iloc[tradeable_sectors].values)
    foreign_price_shock_results['main_minus_common'] = (
        foreign_price_shock_results['main'].astype(float) -
        foreign_price_shock_results['common_theta'].astype(float)
    )
    foreign_price_shock_results['main_closed_minus_common_closed'] = (
        foreign_price_shock_results['main_closed'].astype(float) -
        foreign_price_shock_results['common_theta_closed'].astype(float)
    )
    foreign_price_shock_results.to_csv('clean_data/foreign_price_shock_consumption.csv', index=True)
    for cn in ['main', 'common_theta']:
        sectoral_price_responses[cn].to_csv(f'clean_data/foreign_price_shock_sectoral_prices_{cn}.csv', index=True)
    print("\nForeign price shock results saved.")

# ============================================================================
# 3. Calibrated Domestic Productivity Shocks
# ============================================================================
if RUN_CALIBRATED:
    print("\n" + "="*60)
    print("CALIBRATED DOMESTIC PRODUCTIVITY SHOCKS")
    print("="*60)

    # Draw TFP shocks
    industry_tfp_quadrennial = industry_tfp.pivot(index='Code', columns='year', values='delta_tfp_4').to_numpy()
    industry_tfp_vcov_quadrennial = np.cov(industry_tfp_quadrennial)
    industry_tfp_mean_quadrennial = -np.diag(industry_tfp_vcov_quadrennial) / 2
    industry_tfp_vcov_quadrennial = np.diag(np.diag(industry_tfp_vcov_quadrennial))

    rng = np.random.default_rng(42)
    tfp_samples = multivariate_normal.rvs(
        mean=industry_tfp_mean_quadrennial,
        cov=industry_tfp_vcov_quadrennial,
        size=1000, random_state=rng
    )

    calibrated_configs = ['cobbdouglas', 'cobbdouglas_closed', 'main', 'main_closed', 'common_theta', 'common_theta_closed']
    calibration_shocks_results = pd.DataFrame(index=range(1000), columns=calibrated_configs)
    sectoral_output_results = {cn: pd.DataFrame(index=range(1000), columns=[f'sector_{i}' for i in range(N)]) for cn in calibrated_configs}
    sectoral_price_results = {cn: pd.DataFrame(index=range(1000), columns=[f'sector_{i}' for i in range(N)]) for cn in calibrated_configs}

    for config_name in calibrated_configs:
        print(f"\nConfiguration: {config_name}")
        params = make_params(ALL_CONFIGS[config_name])
        model = SOEModel(params, verbose=True)

        for draw_idx in range(1000):
            params.Z = np.exp(tfp_samples[draw_idx, :])
            solved = model.solve(verbose=False, method='minimize')
            calibration_shocks_results.loc[draw_idx, config_name] = solved['C']
            sectoral_output_results[config_name].loc[draw_idx, :] = solved['Y_d']
            sectoral_price_results[config_name].loc[draw_idx, :] = solved['P_d']
            if (draw_idx + 1) % 100 == 0:
                print(f"  Draw {draw_idx + 1}/1000 completed. C = {solved['C']:.6f}")
            params.Z = np.ones(N)

    calibration_shocks_results.to_csv('clean_data/calibration_shocks_results.csv', index=True)
    for cn in calibrated_configs:
        sectoral_output_results[cn].to_csv(f'clean_data/sectoral_outputs_{cn}.csv', index=True)
        sectoral_price_results[cn].to_csv(f'clean_data/sectoral_prices_{cn}.csv', index=True)
    print("\nCalibrated shocks results saved.")

print("\nAll done!")
