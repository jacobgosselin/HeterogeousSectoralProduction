import numpy as np
import pandas as pd
from scipy.optimize import minimize
from numba import njit
from typing import Tuple, Dict, Optional

def estimate_elasticities_gmm(
    df: pd.DataFrame,
    theta_start: float = .5,
    gamma_start: float = 1.5,
    gamma_type: str = "j-specific",
    theta_type: str = "i-specific"
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Estimate the model:
    y_ijt = (1 - theta_i) * p_jt + (gamma - theta_i)/(gamma - 1) * s_ijt + xi_it + epsilon_ijt

    Using GMM with moment conditions based on industry-specific instruments:
    - Price moments:
      * theta_type="i-specific": E[epsilon_ijt * p_jt * I[i=I]] = 0 for each I (n_i moments)
      * theta_type="common": E[epsilon_ijt * p_jt] = 0 (1 aggregate moment)
    - Share moments (if gamma_type != "none"):
      * theta_type="i-specific": E[epsilon_ijt * s_jt * I[i=I]] = 0 for each I (n_i moments)
      * theta_type="common": E[epsilon_ijt * s_jt] = 0 (1 aggregate moment)

    First demeans all variables by (i,t) to absorb xi_it fixed effects,
    then estimates theta_i and gamma_j (or common parameters).

    Parameters
    ----------
    df : pd.DataFrame
        Must contain columns: i, j, t, y_ijt, p_jt, s_ijt, tradeable (0/1 for j)
    theta_start : float
        Starting value for theta parameters
    gamma_start : float
        Starting value for gamma parameters
    gamma_type : str
        Type of gamma estimation:
        - "none": Closed economy, no coefficient on s_ijt
        - "common": Single gamma for all tradeable sectors
        - "j-specific": One gamma per tradeable sector
    theta_type : str
        Type of theta estimation:
        - "common": Single theta for all industries
        - "i-specific": One theta per industry

    Returns
    -------
    results_theta : pd.DataFrame
        Estimates and robust SEs for theta (common or i-specific)
    results_gamma : pd.DataFrame
        Estimates and robust SEs for gamma (none, common, or j-specific)
    """

    # =========================================================================
    # Validate parameters
    # =========================================================================

    valid_gamma_types = ["none", "common", "j-specific"]
    valid_theta_types = ["common", "i-specific"]

    if gamma_type not in valid_gamma_types:
        raise ValueError(f"gamma_type must be one of {valid_gamma_types}, got '{gamma_type}'")

    if theta_type not in valid_theta_types:
        raise ValueError(f"theta_type must be one of {valid_theta_types}, got '{theta_type}'")

    # =========================================================================
    # Setup indices
    # =========================================================================

    df = df.copy()

    # Extract unique industries and times
    industries_i = np.sort(df['i'].unique())
    industries_j = np.sort(df['j'].unique())
    # Identify tradeable j sectors
    tradeable_df = df[['j', 'tradeable']].drop_duplicates().sort_values('j')
    tradeable_mask = tradeable_df['tradeable'].values == 1
    tradeable_j = industries_j[tradeable_mask]

    n_tradeable = len(tradeable_j)
    n_i = len(industries_i)

    # =========================================================================
    # Demean all variables by (i, t) to absorb xi_it fixed effects
    # =========================================================================

    # Compute within-(i,t) means
    df['y_ijt_mean'] = df.groupby(['i', 't'])['y_ijt'].transform('mean')
    df['p_jt_mean'] = df.groupby(['i', 't'])['p_jt'].transform('mean')
    df['s_ijt_mean'] = df.groupby(['i', 't'])['s_ijt'].transform('mean')

    # Demean
    df['y_demeaned'] = df['y_ijt'] - df['y_ijt_mean']
    df['p_demeaned'] = df['p_jt'] - df['p_jt_mean']
    df['s_demeaned'] = df['s_ijt'] - df['s_ijt_mean']

    # Diagnostic: Check for variation in demeaned data
    print("\n--- DATA DIAGNOSTICS (After Demeaning) ---")
    print(f"y_demeaned: std={df['y_demeaned'].std():.6f}, mean={df['y_demeaned'].mean():.6e}")
    print(f"p_demeaned: std={df['p_demeaned'].std():.6f}, mean={df['p_demeaned'].mean():.6e}")
    print(f"s_demeaned: std={df['s_demeaned'].std():.6f}, mean={df['s_demeaned'].mean():.6e}")

    # Create numeric indices in df
    i_map = {ind: idx for idx, ind in enumerate(industries_i)}
    j_map = {ind: idx for idx, ind in enumerate(industries_j)}
    tradeable_j_map = {ind: idx for idx, ind in enumerate(tradeable_j)}

    df['i_idx'] = df['i'].map(i_map)
    df['j_idx'] = df['j'].map(j_map)

    # Map j to tradeable index (NaN for non-tradeable)
    df['j_tradeable_idx'] = df['j'].map(tradeable_j_map)

    # Extract demeaned data as numpy arrays for speed
    y = df['y_demeaned'].values
    p = df['p_demeaned'].values
    s = df['s_demeaned'].values
    tradeable_arr = df['tradeable'].values
    i_idx = df['i_idx'].values.astype(int)
    j_tradeable_idx = df['j_tradeable_idx'].values  # NaN for non-tradeable

    n_obs = len(df)

    # =========================================================================
    # Parameter and moment layout
    # =========================================================================

    # set number of parameters by theta/gamma types
    n_theta = 1 if theta_type == "common" else n_i
    if gamma_type == "none":
        n_gamma = 0
    elif gamma_type == "common":
        n_gamma = 1
    else:
        n_gamma = n_tradeable
    n_params = n_theta + n_gamma

    # Moment conditions: E[epsilon * instrument] = 0
    # Price moments: E[epsilon * p * I[i=I]] = 0
    #   - theta_type="i-specific": one moment per industry I (n_i moments)
    #   - theta_type="common": one aggregate moment (1 moment)
    # Share moments: E[epsilon * s * I[i=I]] = 0
    #   - gamma_type="none": no share moments (0 moments)
    #   - theta_type="i-specific" (gamma_type != "none"): one moment per industry I (n_i moments)
    #   - theta_type="common" (gamma_type != "none"): one aggregate moment (1 moment)

    # Price moments
    n_moments_p = 1 if theta_type == "common" else n_i

    # Share moments
    if gamma_type == "none":
        n_moments_s = 0
    else:
        n_moments_s = 1 if theta_type == "common" else n_i

    n_moments = n_moments_p + n_moments_s

    theta_slice = slice(0, n_theta)
    gamma_slice = slice(n_theta, n_params)

    # Provide GMM setup information
    print("\n--- GMM SETUP ---")
    print(f"Number of parameters to estimate: {n_params} (theta: {n_theta}, gamma: {n_gamma})")
    print(f"Number of moment conditions: {n_moments} (price moments: {n_moments_p}; share moments: {n_moments_s})")
    print(f"Number of observations: {n_obs}")

    @njit
    def calc_residuals_jit(params, y, p, s, i_idx, j_tradeable_idx, tradeable_arr,
                           n_theta, n_obs, theta_type_code, gamma_type_code):
        """
        JIT-compiled residual calculation.

        theta_type_code: 0 = common, 1 = i-specific
        gamma_type_code: 0 = none, 1 = common, 2 = j-specific
        """
        # Extract parameters
        theta_params = params[:n_theta]
        gamma_params = params[n_theta:]

        # Coefficient on price: (1 - theta_i)
        if theta_type_code == 0:
            theta_i = np.full(n_obs, theta_params[0])
        else:
            theta_i = theta_params[i_idx]
        coef_p = 1.0 - theta_i

        # Coefficient on share: (gamma_j - theta_i) / (gamma_j - 1)
        coef_s = np.zeros(n_obs)
        if gamma_type_code > 0:
            for obs_idx in range(n_obs):
                if tradeable_arr[obs_idx] == 1:
                    if gamma_type_code == 1:
                        gamma_j_val = gamma_params[0]
                    else:
                        j_trad_idx = int(j_tradeable_idx[obs_idx])
                        gamma_j_val = gamma_params[j_trad_idx]
                    theta_i_val = theta_i[obs_idx]
                    coef_s[obs_idx] = (gamma_j_val - theta_i_val) / (gamma_j_val - 1.0)

        # Residuals given parameters
        fitted = coef_p * p + coef_s * s
        resid = y - fitted
        return resid

    @njit
    def calc_moment_conditions_jit(params, y, p, s, i_idx, j_tradeable_idx,
                                    tradeable_arr, n_theta, n_i, n_obs, n_moments,
                                    n_moments_p, theta_type_code, gamma_type_code):
        """
        JIT-compiled GMM moment conditions calculation.

        Constructs moments as:
        - Price: E[epsilon * p * I[i=I]] for each industry I
        - Share: E[epsilon * s * I[i=I]] for each industry I

        theta_type_code: 0 = common, 1 = i-specific
        gamma_type_code: 0 = none, 1 = common, 2 = j-specific
        """
        # Calculate residuals
        resid = calc_residuals_jit(params, y, p, s, i_idx, j_tradeable_idx, tradeable_arr,
                                   n_theta, n_obs, theta_type_code, gamma_type_code)

        # Initialize moment vector
        moments = np.zeros(n_moments)

        # Price moments: E[epsilon * p * I[i=I]]
        if theta_type_code == 0:  # common theta
            # Aggregate moment: E[epsilon * p]
            sum_val = 0.0
            for obs_idx in range(n_obs):
                sum_val += p[obs_idx] * resid[obs_idx]
            moments[0] = sum_val / n_obs
        else:  # i-specific theta
            for i_val in range(n_i):
                sum_val = 0.0
                for obs_idx in range(n_obs):
                    if i_idx[obs_idx] == i_val:
                        sum_val += p[obs_idx] * resid[obs_idx]
                moments[i_val] = sum_val / n_obs

        # Share moments: E[epsilon * s * I[i=I]]
        if gamma_type_code > 0:  # not "none"
            if theta_type_code == 0:  # common theta
                # Aggregate moment: E[epsilon * s]
                sum_val = 0.0
                for obs_idx in range(n_obs):
                    sum_val += s[obs_idx] * resid[obs_idx]
                moments[n_moments_p] = sum_val / n_obs
            else:  # i-specific theta
                for i_val in range(n_i):
                    sum_val = 0.0
                    for obs_idx in range(n_obs):
                        if i_idx[obs_idx] == i_val:
                            sum_val += s[obs_idx] * resid[obs_idx]
                    moments[n_moments_p + i_val] = sum_val / n_obs

        return moments

    # Encode types for JIT compatibility
    theta_type_code = 0 if theta_type == "common" else 1
    gamma_type_code = {"none": 0, "common": 1, "j-specific": 2}[gamma_type]

    def calc_residuals(params: np.ndarray) -> np.ndarray:
        """Calculate residuals given parameter vector (wrapper for JIT function)."""
        return calc_residuals_jit(params, y, p, s, i_idx, j_tradeable_idx, tradeable_arr,
                                  n_theta, n_obs, theta_type_code, gamma_type_code)

    def calc_moment_conditions(params: np.ndarray) -> np.ndarray:
        """Calculate GMM moment conditions (wrapper for JIT function)."""
        return calc_moment_conditions_jit(params, y, p, s, i_idx, j_tradeable_idx,
                                          tradeable_arr, n_theta, n_i, n_obs, n_moments,
                                          n_moments_p, theta_type_code, gamma_type_code)

    def compute_moment_contributions(resid: np.ndarray) -> np.ndarray:
        """
        Compute moment contributions for each observation (n_obs x n_moments matrix).
        Each contribution is the instrument times the residual divided by n_obs.
        The sum of contributions equals the moment vector.

        Parameters
        ----------
        resid : np.ndarray
            Residuals for each observation (n_obs,)

        Returns
        -------
        moment_contributions : np.ndarray
            Matrix of moment contributions (n_obs x n_moments)
            S = moment_contributions.T @ moment_contributions
        """
        moment_contributions = np.zeros((n_obs, n_moments))

        # Price moment contributions: E[epsilon * p * I[i=I]]
        if theta_type == "common":
            moment_contributions[:, 0] = (p * resid) 
        else:  
            for i_val in range(n_i):
                mask = (i_idx == i_val)
                moment_contributions[mask, i_val] = (p[mask] * resid[mask]) 

        # Share moment contributions: E[epsilon * s * I[i=I]]
        if gamma_type != "none":
            if theta_type == "common":
                moment_contributions[:, n_moments_p] = (s * resid) 
            else:  
                for i_val in range(n_i):
                    mask = (i_idx == i_val)
                    moment_contributions[mask, n_moments_p + i_val] = (s[mask] * resid[mask])

        return moment_contributions

    def gmm_objective(params: np.ndarray) -> float:
        """
        GMM objective function with identity weighting: g(beta)' g(beta)
        """
        moments = calc_moment_conditions(params)
        return np.sum(moments**2)

    # =========================================================================
    # Estimation
    # =========================================================================

    # Initial parameter vector
    start_params = np.concatenate([
        np.full(n_theta, theta_start),
        np.full(n_gamma, gamma_start)
    ])

    # Set bounds: theta >= 0, gamma > 0 (bound at top, otherwise poor behavior)
    bounds = [(0, None) for _ in range(n_theta)]  # theta bounds
    if n_gamma > 0:
        bounds += [(1, 100) for _ in range(n_gamma)]  # gamma bounds

    print("\n" + "=" * 60)
    print("Estimating GMM with identity weighting (W = I)...")
    print("=" * 60)
    result = minimize(
        gmm_objective,
        start_params,
        method='Powell',
        bounds=bounds,
        options={'disp': False, 'maxiter': 100000}
    )

    if not result.success:
        error_msg = (
            f"GMM optimization failed to converge.\n"
            f"Optimizer message: {result.message}\n"
            f"Final objective value: {result.fun if hasattr(result, 'fun') else 'N/A'}\n"
            f"Consider adjusting starting values or parameter bounds."
        )
        raise RuntimeError(error_msg)

    beta_hat = result.x
    resid = calc_residuals(beta_hat)
    moments = calc_moment_conditions(beta_hat)

    print(f"Sum of squared moments: {moments @ moments:.2e}")
    print(f"Max abs moment: {np.max(np.abs(moments)):.2e}")

    # =========================================================================
    # Compute standard errors using sandwich formula
    # =========================================================================

    print("\nComputing standard errors...")

    # Compute moment contributions using final residuals
    moment_contributions = compute_moment_contributions(resid)

    # Compute Jacobian G = dg/dbeta (n_moments x n_params matrix)
    print("Computing Jacobian of moment conditions...")
    eps = 1e-9
    G = np.zeros((n_moments, n_params))
    for k in range(n_params):
        params_plus = beta_hat.copy()
        params_plus[k] += eps
        params_minus = beta_hat.copy()
        params_minus[k] -= eps
        G[:, k] = (calc_moment_conditions(params_plus) - calc_moment_conditions(params_minus)) / (2 * eps)

    # Compute variance-covariance matrix using heteroskedastic-robust sandwich formula
    S = 1/n_obs * (moment_contributions.T @ moment_contributions) 

    # Identity weighting (W = I)
    # Robust variance: Var(beta) = (G' G)^{-1} G' S G (G' G)^{-1}
    GtG = G.T @ G
    GtG_inv = np.linalg.inv(GtG)
    middle = G.T @ S @ G
    vcov = 1/n_obs * GtG_inv @ middle @ GtG_inv 

    se = np.sqrt(np.diag(vcov))

    # =========================================================================
    # Parse results
    # =========================================================================

    theta_hat = beta_hat[theta_slice]
    gamma_hat = beta_hat[gamma_slice]

    se_theta = se[theta_slice]
    se_gamma = se[gamma_slice]

    # Build output dataframes
    if theta_type == "common":
        results_theta = pd.DataFrame({
            'i': ['common'],
            'theta_i': theta_hat,
            'se_robust': se_theta,
            't_stat': theta_hat / se_theta
        })
    else:
        results_theta = pd.DataFrame({
            'i': industries_i,
            'theta_i': theta_hat,
            'se_robust': se_theta,
            't_stat': theta_hat / se_theta
        })

    if gamma_type == "none":
        results_gamma = pd.DataFrame({
            'j': [],
            'gamma_j': [],
            'se_robust': [],
            't_stat': []
        })
    elif gamma_type == "common":
        results_gamma = pd.DataFrame({
            'j': ['common'],
            'gamma_j': gamma_hat,
            'se_robust': se_gamma,
            't_stat': gamma_hat / se_gamma
        })
    else:
        results_gamma = pd.DataFrame({
            'j': tradeable_j,
            'gamma_j': gamma_hat,
            'se_robust': se_gamma,
            't_stat': gamma_hat / se_gamma
        })

    # Print summary
    print(f"\nTheta estimates:")
    if theta_type == "common":
        print(results_theta.to_string(index=False))
    else:
        print(results_theta.head(10).to_string(index=False))
        if len(results_theta) > 10:
            print(f"... ({len(results_theta) - 10} more)")

    if gamma_type != "none":
        print(f"\nGamma estimates:")
        if gamma_type == "common":
            print(results_gamma.to_string(index=False))
        else:
            print(results_gamma.head(10).to_string(index=False))
            if len(results_gamma) > 10:
                print(f"... ({len(results_gamma) - 10} more)")

    return results_theta, results_gamma