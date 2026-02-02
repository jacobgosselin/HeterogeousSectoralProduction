"""
3c_quant_results_closed.py

Same tables and figures as 3b_quant_results.py but for the closed economy
calibrations: 'main_closed' and 'common_theta_closed'.
Exports have '_closed' appended to filenames.
"""

import sys
sys.path.insert(0, 'code')

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from helper_fncts.soe_model import *

# ============================================================================
# Load calibration data
# ============================================================================
ro.r['load']('clean_data/calibration_data.RData')

with localconverter(ro.default_converter + pandas2ri.converter):
    Omega = ro.r['Omega_mat']
    Delta = ro.r['Delta_mat']
    calibration_data = ro.r['calibration_vectors']

N = calibration_data.shape[0]
omega_mat = Omega.copy()
omega_mat[omega_mat < 1e-2] = 0
omega_mat = omega_mat / omega_mat.sum(axis=1, keepdims=True)

alpha_vec = calibration_data['alpha_it'].to_numpy()
beta_vec = calibration_data['beta_it'].to_numpy()

xi_mat = np.eye(N) - (omega_mat.T * (1 - alpha_vec))
xi_inv = np.linalg.solve(xi_mat, np.eye(N))
L_vec = alpha_vec * (xi_inv @ beta_vec)

all_industry_names = calibration_data['Industry'].tolist()

# ============================================================================
# 1. Severe Domestic Productivity Shocks Table (Closed)
# ============================================================================
print("Generating severe domestic productivity shocks table (closed)...")

severe_shocks = pd.read_csv('clean_data/severe_shocks_results.csv', index_col=0)

def truncate_industry_50(name):
    return name[:50] + '...' if len(name) > 50 else name

full_table = pd.DataFrame()
full_table['Industry'] = severe_shocks['Industry'].apply(truncate_industry_50)
full_table['Main'] = ((severe_shocks['main_closed'] - 1) * 100).apply(lambda x: f'{x:.2f}\\%')
full_table['Uniform'] = ((severe_shocks['common_theta_closed'] - 1) * 100).apply(lambda x: f'{x:.2f}\\%')
full_table['Change'] = (severe_shocks['main_closed'] - severe_shocks['common_theta_closed']) * 100

full_table_sorted = full_table.sort_values(by='Change', ascending=False)
full_table_sorted['Change'] = full_table_sorted['Change'].apply(lambda x: f'{x:.2f}\\%')
top_3_table = full_table_sorted.head(3)
bottom_3_table = full_table_sorted.tail(3)

combined_table = pd.concat([top_3_table, bottom_3_table], ignore_index=True)
combined_table_tex = combined_table.to_latex(index=False, escape=False)
lines = combined_table_tex.splitlines()
lines.insert(4, r'\midrule')
lines.insert(5, r'\multicolumn{2}{l}{\textbf{Smaller GDP loss}} \\')
lines.insert(6, r'\midrule')
lines.insert(10, r'\midrule')
lines.insert(11, r'\multicolumn{2}{l}{\textbf{Larger GDP loss}} \\')
lines.insert(12, r'\midrule')
latex_table = '\n'.join(lines)
with open("tables/severe_shocks_GDP_closed.tex", "w") as f:
    f.write(latex_table)
print("  Saved tables/severe_shocks_GDP_closed.tex")

# ============================================================================
# 2. Aggregate Responses (Calibrated Shocks, Closed) - Figures & Table
# ============================================================================
print("Generating aggregate response figures and table (closed)...")

calibration_shocks_results = pd.read_csv('clean_data/calibration_shocks_results.csv', index_col=0)

color_main = '#1f77b4'
color_uniform = '#ff7f0e'
color_cobbdouglas = '#2ca02c'

mean_main = calibration_shocks_results['main_closed'].mean()
mean_common = calibration_shocks_results['common_theta_closed'].mean()
mean_cobbdouglas = calibration_shocks_results['cobbdouglas_closed'].mean()

loss_main = (mean_main - 1) * 100
loss_common = (mean_common - 1) * 100
loss_cobbdouglas = (mean_cobbdouglas - 1) * 100

# Plot 1: With Cobb-Douglas
plt.figure(figsize=(8, 6))
sns.kdeplot(calibration_shocks_results['main_closed'], label='Main', fill=True, alpha=0.5, linewidth=2, color=color_main)
sns.kdeplot(calibration_shocks_results['common_theta_closed'], label='Uniform', fill=True, alpha=0.5, linewidth=2, color=color_uniform)
sns.kdeplot(calibration_shocks_results['cobbdouglas_closed'], label='Cobb-Douglas', fill=True, alpha=0.5, linewidth=2, color=color_cobbdouglas)
plt.xlabel('GDP (relative to baseline = 1)', fontsize=18)
plt.ylabel('Density', fontsize=18)
plt.xticks(fontsize=18)
plt.yticks(fontsize=18)
plt.legend(fontsize=18)
plt.text(0.01, 0.95, f'Main: {loss_main:.3f}%\nUniform: {loss_common:.3f}%\nCobb-Douglas: {loss_cobbdouglas:.3f}%',
         horizontalalignment='left', verticalalignment='top', transform=plt.gca().transAxes,
         bbox=dict(boxstyle="round,pad=0.3", edgecolor='black', facecolor='lightgray', alpha=0.5), fontsize=16)
plt.tight_layout()
plt.savefig('figures/aggregate_responses_with_cd_closed.pdf', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved figures/aggregate_responses_with_cd_closed.pdf")

# Plot 2: Without Cobb-Douglas
plt.figure(figsize=(8, 6))
sns.kdeplot((calibration_shocks_results['main_closed'] - 1) * 100, label='Main', fill=True, alpha=0.5, linewidth=3, color=color_main)
sns.kdeplot((calibration_shocks_results['common_theta_closed'] - 1) * 100, label='Uniform', fill=True, alpha=0.5, linewidth=3, color=color_uniform)
plt.xlabel('GDP Response (%)', fontsize=18)
plt.ylabel('Density', fontsize=18)
plt.xticks(fontsize=18)
plt.yticks(fontsize=18)
plt.legend(fontsize=18)
plt.text(0.01, 0.95, f'Main: {loss_main:.2f}%\nUniform: {loss_common:.2f}%',
         horizontalalignment='left', verticalalignment='top', transform=plt.gca().transAxes,
         bbox=dict(boxstyle="round,pad=0.3", edgecolor='black', facecolor='lightgray', alpha=0.5), fontsize=18)
plt.tight_layout()
plt.savefig('figures/aggregate_responses_closed.pdf', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved figures/aggregate_responses_closed.pdf")

# Stats table
def compute_stats(series):
    return series.mean(), series.std(), series.skew()

rows = []
for config_name, label in [('main_closed', 'Main'), ('common_theta_closed', 'Uniform')]:
    mean, std_dev, skewness = compute_stats((calibration_shocks_results[config_name] - 1) * 100)
    rows.append({
        'Calibration': label,
        'Mean': f'{mean:.2f}\%',
        'Std Dev': f'{std_dev:.2f}\%',
        'Skewness': f'{skewness:.2f}\%'
    })
stats_table = pd.DataFrame(rows)
stats_table.to_latex('tables/calibrated_shocks_GDP_closed.tex', index=False, escape=False)
print("  Saved tables/calibrated_shocks_GDP_closed.tex")

# ============================================================================
# 3. Normalize Sectoral Results & Generate Sectoral Tables (Closed)
# ============================================================================
print("Normalizing sectoral results (closed)...")

configs_for_normalization = {
    'cobbdouglas_closed': {'delta': np.ones_like(Delta), 'theta': np.ones_like(calibration_data['theta_i'].to_numpy())},
    'main_closed': {'delta': np.ones_like(Delta), 'theta': calibration_data['theta_i'].to_numpy()},
    'common_theta_closed': {'delta': np.ones_like(Delta), 'theta': calibration_data['theta_uniform'].to_numpy()},
}

sectoral_output_results = {}
sectoral_price_results = {}
for cn in configs_for_normalization:
    sectoral_output_results[cn] = pd.read_csv(f'clean_data/sectoral_outputs_{cn}.csv', index_col=0)
    sectoral_price_results[cn] = pd.read_csv(f'clean_data/sectoral_prices_{cn}.csv', index_col=0)

for cn, config in configs_for_normalization.items():
    params = create_example_parameters(N=66)
    params.omega = omega_mat
    params.alpha = alpha_vec
    params.beta = beta_vec
    params.L = L_vec
    params.epsilon = 0.6
    params.sigma = calibration_data['sigma'].mean()
    params.gamma = calibration_data['gamma_j'].to_numpy()
    params.delta = config['delta'].copy()
    params.theta = config['theta'].copy()
    params.Z = np.ones(N)
    model = SOEModel(params, verbose=False)
    baseline_solved = model.solve(verbose=False, method='minimize')
    sectoral_output_results[cn] = sectoral_output_results[cn] / baseline_solved['Y_d']
    sectoral_price_results[cn] = sectoral_price_results[cn] / baseline_solved['P_d']
    print(f"  Normalized {cn}")

# ============================================================================
# 4. Sectoral Price Table (Top 3, Closed)
# ============================================================================
print("Generating sectoral price table (closed)...")

price_comparison_stats = pd.DataFrame(index=range(N))
for sector_idx in range(N):
    price_comparison_stats.loc[sector_idx, 'mean_main'] = sectoral_price_results['main_closed'][f'sector_{sector_idx}'].mean()
    price_comparison_stats.loc[sector_idx, 'mean_common_theta'] = sectoral_price_results['common_theta_closed'][f'sector_{sector_idx}'].mean()

price_comparison_stats.insert(0, 'Industry', calibration_data['Industry'].values)
price_comparison_stats['main_minus_common'] = price_comparison_stats['mean_main'] - price_comparison_stats['mean_common_theta']

top_3_data = price_comparison_stats.sort_values(by='main_minus_common', ascending=False).head(3)
top_3_price_table = pd.DataFrame()
top_3_price_table['Industry'] = top_3_data['Industry'].apply(lambda x: x[:75] + '...' if len(x) > 75 else x).values
top_3_price_table['Main'] = ((top_3_data['mean_main'] - 1) * 100).apply(lambda x: f'{x:.2f}\\%').values
top_3_price_table['Uniform'] = ((top_3_data['mean_common_theta'] - 1) * 100).apply(lambda x: f'{x:.2f}\\%').values
top_3_price_table['Difference'] = (top_3_data['main_minus_common'] * 100).apply(lambda x: f'{x:.2f}\\%').values
top_3_price_table.to_latex('tables/calibrated_shocks_top3_closed.tex', index=False, escape=False)
print("  Saved tables/calibrated_shocks_top3_closed.tex")

print("\nAll closed-economy tables and figures exported.")
