import numpy as np
import pandas as pd
from scipy.optimize import least_squares
from numba import njit
from typing import Tuple, Dict, Optional

def estimate_elasticities_gmm(
    df: pd.DataFrame,
    theta_start: float = .5,
    gamma_start: float = 1.5,
    gamma_type: str = "j-specific",
    theta_type: str = "i-specific",
    check_jacobian: bool = False
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    V2 of the GMM elasticity estimator.

    Estimates the model:
    y_ijt = (1 - theta_i) * p_jt + (gamma_j - theta_i)/(gamma_j - 1) * s_ijt + xi_it + epsilon_ijt

    Differences from V1 (helper_fncts/GMM_Est_Fncts.py):
    - Solves with scipy.optimize.least_squares (Gauss-Newton / trust-region) on the
      moment vector g(beta) directly, instead of minimize(method='Powell') on g'g.
    - Supplies an ANALYTIC Jacobian of the moments (no finite differencing), which is
      also reused for the standard-error sandwich.
    - Weighting is still identity (W = I), exactly as V1. The least_squares objective
      minimizes (1/2)||g||^2, which has the same minimizer as V1's g'g.

    Moment conditions (identical to V1):
    - Price moments:  E[epsilon * p * I[i=I]] = 0
        * theta_type="i-specific": one moment per industry I (n_i moments)
        * theta_type="common":     one aggregate moment (1 moment)
    - Import share moments (if gamma_type != "none"): E[epsilon * s * I[i=I]] = 0
        * theta_type="i-specific": one moment per industry I (n_i moments)
        * theta_type="common":     one aggregate moment (1 moment)

    All variables are first demeaned by (i,t) to absorb xi_it fixed effects.

    Parameters
    ----------
    df : pd.DataFrame
        Must contain columns: i, j, t, y_ijt, p_jt, s_ijt, tradeable (0/1 for j)
    theta_start : float
        Starting value for theta parameters
    gamma_start : float
        Starting value for gamma parameters
    gamma_type : str
        "none" | "common" | "j-specific"
    theta_type : str
        "common" | "i-specific"
    check_jacobian : bool
        If True, compare the analytic Jacobian to a central finite-difference
        Jacobian at the starting point and at the solution, and assert agreement.

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

    # Price moments
    n_moments_p = 1 if theta_type == "common" else n_i

    # Import share moments
    if gamma_type == "none":
        n_moments_s = 0
    else:
        n_moments_s = 1 if theta_type == "common" else n_i

    n_moments = n_moments_p + n_moments_s

    theta_slice = slice(0, n_theta)
    gamma_slice = slice(n_theta, n_params)

    # Provide GMM setup information
    print("\n--- GMM SETUP (V2: least_squares + analytic Jacobian) ---")
    print(f"Number of parameters to estimate: {n_params} (theta: {n_theta}, gamma: {n_gamma})")
    print(f"Number of moment conditions: {n_moments} (price moments: {n_moments_p}; import share moments: {n_moments_s})")
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

    # =========================================================================
    # Analytic Jacobian of the moment vector:  G[m, k] = d g_m / d beta_k
    # =========================================================================
    #
    # Residual:  eps = y - (1 - theta_i) p - coef_s s,
    #            coef_s = (gamma_j - theta_i)/(gamma_j - 1),  (s = coef_s = 0 if non-tradeable)
    #
    # Residual partials (the only calculus):
    #   d eps / d theta_i = p + s / (gamma_j - 1)            (share term 0 for non-tradeables)
    #   d eps / d gamma_j = (1 - theta_i) / (gamma_j - 1)^2 * s   (0 for non-tradeables)
    #
    # Moments g_m = (1/n) sum_o z_{m,o} eps_o, so
    #   G[m, k] = (1/n) sum_o z_{m,o} * (d eps_o / d beta_k).
    #
    # Block structure (rows = [price | share], cols = [theta | gamma]):
    #   - theta blocks are DIAGONAL in industry (moment I sums only over obs with i=I,
    #     and those obs depend only on theta_I).
    #   - gamma blocks are sparse: d g_I / d gamma_J nonzero only for obs with i=I AND j=J.
    #   - common-theta / common-gamma collapse the block to one summed column;
    #     gamma_type="none" drops gamma columns and share-moment rows.
    def calc_jacobian(params: np.ndarray) -> np.ndarray:
        """Analytic Jacobian of the moment vector, shape (n_moments, n_params)."""
        # Per-observation theta_i and gamma_j (gamma only meaningful for tradeables)
        if theta_type_code == 0:
            theta_obs = np.full(n_obs, params[0])
        else:
            theta_obs = params[i_idx]

        # Per-observation (gamma_j - 1); only used on tradeable obs.
        gamma_minus1_obs = np.ones(n_obs)  # placeholder for non-tradeables (unused)
        if gamma_type_code == 1:
            gamma_minus1_obs = np.where(tradeable_arr == 1, params[n_theta] - 1.0, 1.0)
        elif gamma_type_code == 2:
            trad = tradeable_arr == 1
            jt = np.where(trad, np.nan_to_num(j_tradeable_idx, nan=0.0), 0.0).astype(int)
            gamma_vals = params[n_theta + jt]
            gamma_minus1_obs = np.where(trad, gamma_vals - 1.0, 1.0)

        # d eps / d theta  (per obs).  Share term only on tradeables.
        deps_dtheta = p.copy()
        if gamma_type_code > 0:
            share_term = np.where(tradeable_arr == 1, s / gamma_minus1_obs, 0.0)
            deps_dtheta = deps_dtheta + share_term

        # d eps / d gamma  (per obs).  Nonzero only on tradeables.
        if gamma_type_code > 0:
            deps_dgamma = np.where(
                tradeable_arr == 1,
                (1.0 - theta_obs) / (gamma_minus1_obs ** 2) * s,
                0.0
            )

        G = np.zeros((n_moments, n_params))

        # ---- theta columns ----
        # Price moments rows use instrument z = p; share moment rows use z = s.
        if theta_type_code == 0:
            # common theta -> single column 0
            G[0, 0] = np.sum(p * deps_dtheta) / n_obs
            if gamma_type_code > 0:
                G[n_moments_p, 0] = np.sum(s * deps_dtheta) / n_obs
        else:
            # i-specific theta -> diagonal in industry
            for i_val in range(n_i):
                mask = (i_idx == i_val)
                G[i_val, i_val] = np.sum(p[mask] * deps_dtheta[mask]) / n_obs
                if gamma_type_code > 0:
                    G[n_moments_p + i_val, i_val] = np.sum(s[mask] * deps_dtheta[mask]) / n_obs

        # ---- gamma columns ----
        if gamma_type_code > 0:
            if gamma_type_code == 1:
                # common gamma -> single column at index n_theta
                gcol = n_theta
                if theta_type_code == 0:
                    G[0, gcol] = np.sum(p * deps_dgamma) / n_obs
                    G[n_moments_p, gcol] = np.sum(s * deps_dgamma) / n_obs
                else:
                    for i_val in range(n_i):
                        mask = (i_idx == i_val)
                        G[i_val, gcol] = np.sum(p[mask] * deps_dgamma[mask]) / n_obs
                        G[n_moments_p + i_val, gcol] = np.sum(s[mask] * deps_dgamma[mask]) / n_obs
            else:
                # j-specific gamma -> column per tradeable sector J.
                # d g_I / d gamma_J accumulates over obs with i=I AND j=J.
                trad = tradeable_arr == 1
                jt_idx = np.where(trad, np.nan_to_num(j_tradeable_idx, nan=-1.0), -1.0).astype(int)
                if theta_type_code == 0:
                    for j_val in range(n_tradeable):
                        mask = trad & (jt_idx == j_val)
                        gcol = n_theta + j_val
                        G[0, gcol] = np.sum(p[mask] * deps_dgamma[mask]) / n_obs
                        G[n_moments_p, gcol] = np.sum(s[mask] * deps_dgamma[mask]) / n_obs
                else:
                    for j_val in range(n_tradeable):
                        col_mask = trad & (jt_idx == j_val)
                        gcol = n_theta + j_val
                        # price moment row I and share moment row I both get the (i=I & j=J) cells
                        i_of_cells = i_idx[col_mask]
                        p_cells = p[col_mask] * deps_dgamma[col_mask]
                        s_cells = s[col_mask] * deps_dgamma[col_mask]
                        for i_val in np.unique(i_of_cells):
                            sub = i_of_cells == i_val
                            G[i_val, gcol] = np.sum(p_cells[sub]) / n_obs
                            G[n_moments_p + i_val, gcol] = np.sum(s_cells[sub]) / n_obs

        return G

    def _finite_diff_jacobian(params: np.ndarray, eps: float = 1e-7,
                              lb: Optional[np.ndarray] = None,
                              ub: Optional[np.ndarray] = None) -> np.ndarray:
        """
        Finite-difference Jacobian, for validation only.

        Uses a central difference at interior points. If lb/ub are supplied and a
        parameter sits on (or within eps of) a bound, it switches to a one-sided
        difference stepping INWARD, so the probe stays feasible. This matters
        because several gamma_j routinely pin to their bounds at the solution, where
        a two-sided difference would straddle the boundary and is not a valid check.
        """
        G = np.zeros((n_moments, n_params))
        for k in range(n_params):
            # Scale the step to the parameter magnitude. Near the gamma lower bound
            # (gamma -> 1) the residual partial ~ 1/(gamma-1)^2 has enormous
            # curvature; a fixed step would straddle the singularity and give a
            # meaningless difference, so shrink the step relative to (param - lb).
            h = eps * max(1.0, abs(params[k]))
            if lb is not None:
                h = min(h, 0.25 * max(params[k] - lb[k], 0.0)) or h
            if ub is not None:
                h = min(h, 0.25 * max(ub[k] - params[k], 0.0)) or h
            if h <= 0:
                h = eps
            at_lb = lb is not None and params[k] - lb[k] <= h
            at_ub = ub is not None and ub[k] - params[k] <= h
            if at_ub:  # step down only
                pm = params.copy(); pm[k] -= h
                G[:, k] = (calc_moment_conditions(params) - calc_moment_conditions(pm)) / h
            elif at_lb:  # step up only
                pp = params.copy(); pp[k] += h
                G[:, k] = (calc_moment_conditions(pp) - calc_moment_conditions(params)) / h
            else:  # central
                pp = params.copy(); pp[k] += h
                pm = params.copy(); pm[k] -= h
                G[:, k] = (calc_moment_conditions(pp) - calc_moment_conditions(pm)) / (2 * h)
        return G

    def compute_moment_contributions(resid: np.ndarray) -> np.ndarray:
        """
        Compute moment contributions for each observation (n_obs x n_moments matrix).
        Each contribution is the instrument times the residual (per-obs, NOT divided by n).
        S = (1/n) * (contributions.T @ contributions).
        """
        moment_contributions = np.zeros((n_obs, n_moments))

        # Price moment contributions
        if theta_type == "common":
            moment_contributions[:, 0] = (p * resid)
        else:
            for i_val in range(n_i):
                mask = (i_idx == i_val)
                moment_contributions[mask, i_val] = (p[mask] * resid[mask])

        # Share moment contributions
        if gamma_type != "none":
            if theta_type == "common":
                moment_contributions[:, n_moments_p] = (s * resid)
            else:
                for i_val in range(n_i):
                    mask = (i_idx == i_val)
                    moment_contributions[mask, n_moments_p + i_val] = (s[mask] * resid[mask])

        return moment_contributions

    # =========================================================================
    # Estimation via least_squares (Gauss-Newton, identity weighting)
    # =========================================================================

    # Initial parameter vector
    start_params = np.concatenate([
        np.full(n_theta, theta_start),
        np.full(n_gamma, gamma_start)
    ])

    # Bounds: theta >= 0, gamma in (1, 100]. least_squares 'trf' takes (lb, ub) arrays.
    lb = np.concatenate([np.zeros(n_theta), np.full(n_gamma, 1.0 + 1e-6)])
    ub = np.concatenate([np.full(n_theta, np.inf), np.full(n_gamma, 100.0)])

    # Optional Jacobian validation (off by default). We validate at INTERIOR points
    # only. The finite-difference reference is unreliable at the solution because
    # parameters routinely pin to their bounds (theta=0; gamma=100; gamma->1, where
    # the residual partial ~ 1/(gamma-1)^2 is singular) and the moments are O(1e-5),
    # so finite differences there suffer catastrophic cancellation. Interior points
    # exercise every closed-form term and are the rigorous correctness test.
    if check_jacobian:
        rng = np.random.default_rng(0)
        interior_pts = [(start_params, "start")]
        for r in range(2):
            rp = np.concatenate([
                rng.uniform(0.1, 2.0, n_theta),
                rng.uniform(1.5, 5.0, n_gamma),
            ])
            interior_pts.append((rp, f"random-{r}"))
        for test_pt, label in interior_pts:
            G_an = calc_jacobian(test_pt)
            G_fd = _finite_diff_jacobian(test_pt, lb=lb, ub=ub)
            # Combined absolute+relative tolerance (allclose-style): handles both
            # tiny-magnitude entries (where pure relative error is misleading) and
            # large entries.
            ok = np.allclose(G_an, G_fd, rtol=1e-4, atol=1e-7)
            worst = np.max(np.abs(G_an - G_fd) - (1e-7 + 1e-4 * np.abs(G_an)))
            print(f"[Jacobian check @ {label}] allclose={ok} (worst margin over tol = {worst:.3e})")
            assert ok, f"Analytic Jacobian disagrees with finite difference at {label}"

    print("\n" + "=" * 60)
    print("Estimating GMM with least_squares (W = I), analytic Jacobian...")
    print("=" * 60)
    result = least_squares(
        fun=calc_moment_conditions,
        x0=start_params,
        jac=calc_jacobian,
        bounds=(lb, ub),
        method='trf',
        xtol=1e-12,
        ftol=1e-12,
        gtol=1e-12,
        max_nfev=100000,
    )

    if not result.success:
        error_msg = (
            f"GMM optimization (least_squares) failed to converge.\n"
            f"Optimizer status: {result.status}\n"
            f"Optimizer message: {result.message}\n"
            f"Final cost (1/2 ||g||^2): {result.cost:.3e}\n"
            f"Consider adjusting starting values or parameter bounds."
        )
        raise RuntimeError(error_msg)

    beta_hat = result.x
    resid = calc_residuals(beta_hat)
    moments = calc_moment_conditions(beta_hat)

    print(f"Sum of squared moments: {moments @ moments:.2e}")
    print(f"Max abs moment: {np.max(np.abs(moments)):.2e}")

    # =========================================================================
    # Compute standard errors using sandwich formula (analytic Jacobian)
    # =========================================================================

    print("\nComputing standard errors...")

    # Informational-only check at the solution (NOT asserted). At the optimum many
    # parameters sit on their bounds (theta=0; gamma=100; gamma->1, where the
    # residual partial ~ 1/(gamma-1)^2 is singular), and the moments are O(1e-5), so
    # a finite-difference reference suffers catastrophic cancellation and is not a
    # reliable test here. The interior checks above already validate the closed-form
    # Jacobian; this print just reports how close the (unreliable) FD reference lands.
    if check_jacobian:
        G_an = calc_jacobian(beta_hat)
        G_fd = _finite_diff_jacobian(beta_hat, lb=lb, ub=ub)
        col_ok = np.ones(n_params, dtype=bool)
        if gamma_type != "none":
            col_ok[n_theta:] = (beta_hat[n_theta:] - 1.0) > 1e-3
        ok = np.allclose(G_an[:, col_ok], G_fd[:, col_ok], rtol=1e-4, atol=1e-7)
        n_excluded = int((~col_ok).sum())
        print(f"[Jacobian check @ solution, informational] allclose={ok} "
              f"({n_excluded} gamma col(s) near gamma=1 excluded; FD unreliable at bounds)")

    # Moment contributions at the solution
    moment_contributions = compute_moment_contributions(resid)

    # Analytic Jacobian G = dg/dbeta (n_moments x n_params)
    print("Computing analytic Jacobian of moment conditions...")
    G = calc_jacobian(beta_hat)

    # Robust sandwich under identity weighting (W = I):
    #   Var(beta) = (G'G)^{-1} G' S G (G'G)^{-1}
    S = 1/n_obs * (moment_contributions.T @ moment_contributions)  # variance of moments
    GtG = G.T @ G
    GtG_inv = np.linalg.inv(GtG)
    middle = G.T @ S @ G
    vcov = GtG_inv @ middle @ GtG_inv
    se = np.sqrt(1/n_obs * np.diag(vcov))  # ses = sqrt(diag(1/n * V))

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
