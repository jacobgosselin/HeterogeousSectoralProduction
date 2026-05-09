import numpy as np
from numba import njit
import pandas as pd

@njit
def armington_price_index(P_d: float, P_f: float,
                              delta: float, gamma: float) -> float:
        """
        Compute Armington price index: P = [delta * P_d^(1-gamma) + (1-delta) * P_f^(1-gamma)]^(1/(1-gamma))

        Args:
            P_d: domestic price
            P_f: foreign price
            delta: domestic share 
            gamma: Armington elasticity 

        Returns:
            Armington price index
        """
        result = (delta * P_d ** (1 - gamma) + (1 - delta) * P_f ** (1 - gamma))** (1 / (1 - gamma))
        return result

@njit
def intermediate_price_index(P_armington_i: np.ndarray, omega_i: np.ndarray,
                                 theta_i: float) -> float:
        """
        Compute intermediate price index for sector i: Q_i = [sum_j omega_ij * P_j^(1-theta_i)]^(1/(1-theta_i))

        Args:
            P_armington_i: Armington prices for all inputs j used by sector i (N,)
            omega_i: input shares for sector i (N,)
            theta_i: elasticity of substitution for sector i

        Returns:
            Intermediate price index Q_i
        """
        if np.abs(theta_i - 1) < 1e-10:
            # Cobb-Douglas case
            return np.prod(P_armington_i ** omega_i)
        else:
            # CES case
            return (np.sum(omega_i * P_armington_i ** (1 - theta_i))) ** (1 / (1 - theta_i))

@njit
def marginal_cost(W_i: float, Q_i: float, alpha_i: float,
                    epsilon: float, Z_i: float) -> float:
    """
    Compute marginal cost: MC_i = [alpha * W^(1-epsilon) + (1-alpha) * Q^(1-epsilon)]^(1/(1-epsilon)) / Z

    Args:
        W_i: wage in sector i
        Q_i: intermediate price index for sector i
        alpha_i: labor share in sector i
        epsilon: elasticity between labor and intermediates
        Z_i: productivity in sector i

    Returns:
        Marginal cost MC_i
    """
    # Ensure positive inputs to avoid NaN in power operations
    W_i = max(W_i, 1e-10)
    Q_i = max(Q_i, 1e-10)
    Z_i = max(Z_i, 1e-10)

    if np.abs(epsilon - 1) < 1e-10:
        # Cobb-Douglas case
        return (W_i ** alpha_i) * (Q_i ** (1 - alpha_i)) / Z_i
    else:
        # CES case
        return ((alpha_i * W_i ** (1 - epsilon) + (1 - alpha_i) * Q_i ** (1 - epsilon))
                ** (1 / (1 - epsilon))) / Z_i
    
@njit
def intermediate_bundle(Y_i:float, Q_i:float, MC_i:float, 
                        Z_i:float, epsilon:float, alpha_i:float) -> float:
        """
        From the nested CES structure, we have:
        - M_i = (1 - alpha_i) * Z_i^(epsilon - 1) * Y_i * (MC_i / Q_i)^epsilon
        """
        M_i = (1 - alpha_i) * Z_i**(epsilon - 1) * Y_i * (MC_i / Q_i)**epsilon
        return M_i

@njit
def intermediate_composite(M_i:float, Q_i:float, P_armington_ij:float,
                            omega_ij:float, theta_i:float) -> float:
    """
    From the nested CES structure, we have:
    - X_ij = omega_ij * M_i * (Q_i / P_armington_ij)^theta_i
    """
    X_ij = omega_ij * M_i * (Q_i / P_armington_ij)**theta_i 
    return X_ij

@njit 
def intermediate_domestic(X_ij:float, P_armington_ij:float, P_d_j:float,
                            delta_ij:float, gamma_j:float) -> float:
    """
    From the nested CES structure, we have:
    - X_ijd = delta_ij * X_ij * (P_armington_ij / P_d_j)^gamma_j
    """
    X_ijd = delta_ij * X_ij * (P_armington_ij / P_d_j)**gamma_j 
    return X_ijd

@njit
def consumption_composite(beta_i: float, C: float, P_armington_i: float, sigma: float) -> float:
    """
    From nested CES structure, we have:
    P_i = (beta_i C/C_i)^(1/sigma) (normalize P_C=labmda = 1)
    C_i = beta_i * C * (1/ P_armington_i)^sigma
    """
    C_i = beta_i * C * (1 / P_armington_i) ** sigma
    return C_i

@njit
def consumption_domestic(C_i: float, P_armington_i: float, P_d_i: float,
                            delta_0i: float, gamma_i: float) -> float:
    """
    From nested CES structure, we have:
    P_id = P_i (delta_0i * (C_i/C_id))^(1/gamma_i)
    C_id = delta_0i * C_i * (P_armington_i / P_d_i)^gamma_i
    """
    C_id = delta_0i * C_i * (P_armington_i / P_d_i) ** gamma_i
    return C_id

@njit
def compute_wages(P_d: np.ndarray, Y_d: np.ndarray, Z: np.ndarray,
                  epsilon: float, alpha: np.ndarray, L: np.ndarray) -> np.ndarray:
        """
        Step 2 of pseudo-code: Compute W_i from marginal product of labor condition.
        W_i = P_d[i] * Z_i^(1-1/epsilon) * (alpha_i * Y_i / L_i)^(1/epsilon)

        Args:
            P_d: domestic prices (N,)
            Y_d: domestic outputs (N,)
            Z: productivity (N,)
            epsilon: elasticity between labor and intermediates
            alpha: labor shares (N,)
            L: labor endowments (N,)

        Returns:
            Wages W (N,)
        """
        return P_d * Z**(1 - 1/epsilon) * (alpha * Y_d / L)**(1 / epsilon)

@njit
def compute_marginal_costs(P_d: np.ndarray, W: np.ndarray, E: float,
                           P_f: np.ndarray, delta: np.ndarray, gamma: np.ndarray,
                           omega: np.ndarray, theta: np.ndarray, alpha: np.ndarray,
                           epsilon: float, Z: np.ndarray) -> np.ndarray:
    """
    Step 3 of pseudo-code: Compute implied MC_i as CES unit cost function.

    Args:
        P_d: domestic prices (N,)
        W: wages (N,)
        E: exchange rate
        P_f: foreign prices (N,)
        delta: domestic shares for intermediates (N, N)
        gamma: Armington elasticities (N,)
        omega: input-output coefficients (N, N)
        theta: intermediate elasticities (N,)
        alpha: labor shares (N,)
        epsilon: labor-intermediate elasticity
        Z: productivity (N,)

    Returns:
        Marginal costs MC (N,)
    """
    N = P_d.shape[0]
    MC = np.zeros(N)

    for i in range(N):
        # Compute Armington price indices for all inputs used by sector i
        P_armington_i = np.zeros(N)
        for j in range(N):
            P_armington_i[j] = armington_price_index(P_d[j], E * P_f[j], delta[i, j], gamma[j])

        # Compute intermediate price index
        Q_i = intermediate_price_index(P_armington_i, omega[i, :], theta[i])

        # Compute marginal cost
        MC[i] = marginal_cost(W[i], Q_i, alpha[i], epsilon, Z[i])

    return MC

@njit
def compute_intermediate_demands(P_d: np.ndarray, Y_d: np.ndarray, E: float,
                                 P_f: np.ndarray, delta: np.ndarray, gamma: np.ndarray,
                                 omega: np.ndarray, theta: np.ndarray, alpha: np.ndarray,
                                 epsilon: float, Z: np.ndarray, L: np.ndarray) -> np.ndarray:
    """
    Step 4 of pseudo-code: Given Y_i and P_i, compute implied X_ijd from FOC of firm problem.

    Args:
        P_d: domestic prices (N,)
        Y_d: domestic outputs (N,)
        E: exchange rate
        P_f: foreign prices (N,)
        delta: domestic shares for intermediates (N, N)
        gamma: Armington elasticities (N,)
        omega: input-output coefficients (N, N)
        theta: intermediate elasticities (N,)
        alpha: labor shares (N,)
        epsilon: labor-intermediate elasticity
        Z: productivity (N,)
        L: labor endowments (N,)

    Returns:
        X_d: (N, N) matrix where X_d[i,j] = domestic demand by sector i for good j
    """
    N = P_d.shape[0]
    W = compute_wages(P_d, Y_d, Z, epsilon, alpha, L)
    MC = compute_marginal_costs(P_d, W, E, P_f, delta, gamma, omega, theta, alpha, epsilon, Z)
    X_d = np.zeros((N, N))

    for i in range(N):
        # Compute overall intermediate use
        P_armington_i = np.zeros(N)
        for j in range(N):
            P_armington_i[j] = armington_price_index(P_d[j], E * P_f[j], delta[i, j], gamma[j])
        Q_i = intermediate_price_index(P_armington_i, omega[i, :], theta[i])
        M_i = intermediate_bundle(Y_d[i], Q_i, MC[i], Z[i], epsilon, alpha[i])
        # Compute composite intermediate demands and then domestic demands
        for j in range(N):
            X_ij = intermediate_composite(M_i, Q_i, P_armington_i[j], omega[i, j], theta[i])
            X_d[i, j] = intermediate_domestic(X_ij, P_armington_i[j], P_d[j], delta[i, j], gamma[j])
    return X_d

@njit
def compute_consumption_price(P_d: np.ndarray, E: float, P_f: np.ndarray,
                              delta_0: np.ndarray, gamma: np.ndarray,
                              beta: np.ndarray, sigma: float):
    """
    Compute aggregate consumption price index P_C from Armington prices.

    Args:
        P_d: domestic prices (N,)
        E: exchange rate
        P_f: foreign prices (N,)
        delta_0: domestic shares for final consumption (N,)
        gamma: Armington elasticities (N,)
        beta: consumption shares (N,)
        sigma: consumption elasticity

    Returns:
        P_armington: Armington prices (N,)
        P_C: aggregate consumption price index
    """
    N = P_d.shape[0]
    P_armington = np.zeros(N)
    for i in range(N):
        P_armington[i] = armington_price_index(P_d[i], E * P_f[i], delta_0[i], gamma[i])

    if np.abs(sigma - 1) < 1e-10:
        # Cobb-Douglas case
        P_C = np.prod(P_armington ** beta)
    else:
        # CES case
        P_C = (np.sum(beta * P_armington ** (1 - sigma))) ** (1 / (1 - sigma))

    return P_armington, P_C

@njit
def compute_consumption_demands(W: np.ndarray, P_d: np.ndarray, E: float,
                                P_f: np.ndarray, delta_0: np.ndarray, gamma: np.ndarray,
                                beta: np.ndarray, sigma: float, L: np.ndarray,
                                NX: np.ndarray, calibrating: bool):
    """
    Compute consumption demands for all goods.

    Args:
        W: wages (N,)
        P_d: domestic prices (N,)
        E: exchange rate
        P_f: foreign prices (N,)
        delta_0: domestic shares for final consumption (N,)
        gamma: Armington elasticities (N,)
        beta: consumption shares (N,)
        sigma: consumption elasticity
        L: labor endowments (N,)
        NX: net exports position
        calibrating: whether in calibration mode

    Returns:
        C_d: domestic consumption demands (N,)
        P_C: aggregate consumption price index
    """
    P_armington, P_C = compute_consumption_price(P_d, E, P_f, delta_0, gamma, beta, sigma)

    # Total consumption expenditure from wage income
    if calibrating:
        C = 1.0  # during calibration, set C=1
    else:
        C = np.sum(W * L) 

    # Compute consumption demands
    N = P_d.shape[0]
    C_d = np.zeros(N)
    for i in range(N):
        C_composite_i = consumption_composite(beta[i], C, P_armington[i], sigma)
        C_d[i] = consumption_domestic(C_composite_i, P_armington[i], P_d[i], delta_0[i], gamma[i])

    return C_d, P_C

@njit
def compute_residuals(x: np.ndarray, N: int, P_f: np.ndarray, delta: np.ndarray,
                     delta_0: np.ndarray, gamma: np.ndarray, omega: np.ndarray,
                     theta: np.ndarray, alpha: np.ndarray, epsilon: float,
                     Z: np.ndarray, L: np.ndarray, beta: np.ndarray, sigma: float,
                     NX: np.ndarray, calibrating: bool) -> np.ndarray:
    """
    Step 6 of pseudo-code: Compute residuals of 2N+1 equations.

    During calibration (baseline with Z=1, P_f=1):
    - N price equations: P_d[i] = MC[i]
    - N quantity equations: Y_d[i] = C_d[i] + sum_k X_d[k,i]
    - 1 price index equation: P_C = 1

    After calibration (with shocks):
    - N price equations: P_d[i] = MC[i]
    - N quantity equations: Y_d[i] = C_d[i] + sum_k X_d[k,i] + NX_i
    - 1 price index equation: P_C = 1

    Args:
        x: stacked vector [P_d (N), Y_d (N), E (1)]
        N: number of sectors
        P_f: foreign prices (N,)
        delta: domestic shares for intermediates (N, N)
        delta_0: domestic shares for final consumption (N,)
        gamma: Armington elasticities (N,)
        omega: input-output coefficients (N, N)
        theta: intermediate elasticities (N,)
        alpha: labor shares (N,)
        epsilon: labor-intermediate elasticity
        Z: productivity (N,)
        L: labor endowments (N,)
        beta: consumption shares (N,)
        sigma: consumption elasticity
        NX: net exports position
        calibrating: whether in calibration mode

    Returns:
        residuals: (2N+1,) array of residuals
    """
    P_d = np.maximum(x[:N], 1e-10)  # Ensure positive prices
    Y_d = np.maximum(x[N:2*N], 1e-10)  # Ensure positive outputs
    E = np.maximum(x[2*N], 1e-10)  # Exchange rate

    # Step 2: Compute wages
    W = compute_wages(P_d, Y_d, Z, epsilon, alpha, L)

    # Step 3: Compute marginal costs
    MC = compute_marginal_costs(P_d, W, E, P_f, delta, gamma, omega, theta, alpha, epsilon, Z)

    # Step 4: Compute intermediate demands
    X_d = compute_intermediate_demands(P_d, Y_d, E, P_f, delta, gamma, omega, theta, alpha, epsilon, Z, L)

    # Step 5: Compute consumption demands and P_C
    C_d, P_C = compute_consumption_demands(W, P_d, E, P_f, delta_0, gamma, beta, sigma, L, NX, calibrating)

    # Check if closed (all delta = 1)
    closed_economy = np.all(delta == 1.0) and np.all(delta_0 == 1.0)
    if closed_economy:
        residuals = np.zeros(2 * N + 1)
        # N price equations: P_d[i] = MC[i]
        residuals[:N] = P_d - MC
        # N-1 quantity equations (drop last due to Walras's law)
        for i in range(N-1):
            residuals[N + i] = Y_d[i] - C_d[i] - np.sum(X_d[:, i])
        # 1 price normalization: P_C = 1 (pins down price level)
        residuals[2*N - 1] = P_C - 1.0
        # 1 equation to pin down E (irrelevant in closed economy, just set E = 1)
        residuals[2*N] = E - 1.0
    
    else:
        if calibrating:
            # 2N + 1 equations during calibration
            residuals = np.zeros(2 * N + 1)
            # N price equations: P_d[i] = MC[i]
            residuals[:N] = P_d - MC
            # Prices = 1 during calibration
            residuals[N:2*N] = P_d - 1.0
            # Real exchange rate = 1 during calibration
            residuals[2*N] = E - 1.0

        if not calibrating:
            # 2N+1 equations
            residuals = np.zeros(2 * N + 1)
            # N price equations: P_d[i] = MC[i]
            residuals[:N] = P_d - MC
            # N quantity equations
            for i in range(N):
                residuals[N + i] = Y_d[i] - C_d[i] - np.sum(X_d[:, i]) - NX[i]*((1/E)*P_d[i])**(-gamma[i])
            # 1 price index equation: P_C = 1
            residuals[2*N] = 1.0 - P_C

    return residuals


     