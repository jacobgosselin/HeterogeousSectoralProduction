"""
Small Open Economy GE Model Solver

Static GE model with N sectors, nested CES production and consumption,
Armington aggregation for domestic/foreign goods, and fixed labor by sector.

This implementation follows the pseudo-code structure in Model.md.
"""

import numpy as np
from scipy.optimize import least_squares, minimize
from dataclasses import dataclass
from typing import Optional
from helper_fncts.soe_solver_fncts import *
from numba import jit

@dataclass
class ModelParameters:
    """
    Parameters for the small open economy model.

    Dimensions:
        N: number of sectors

    Elasticities:
        sigma: elasticity of substitution across goods in final consumption
        epsilon: elasticity between labor and intermediates in production
        theta: (N,) elasticity across intermediate inputs within a sector
        gamma: (N,) Armington elasticity between domestic and foreign goods (applies to both firms and households)

    Share parameters:
        beta: (N,) consumption shares across sectors
        alpha: (N,) labor share in production (vs intermediates)
        omega: (N, N) input-output coefficients, omega[i,j] = share of input j in sector i's intermediate bundle
        delta: (N, N) domestic share in Armington for intermediates, delta[i,j] for input j used by sector i
        delta_0: (N,) domestic share in Armington for final consumption

    Endowments:
        L: (N,) labor endowment by sector (fixed)
        Z: (N,) productivity by sector

    Foreign prices:
        P_f: (N,) exogenous foreign prices

    Net exports (calibrated):
        NX: (N,) sector-specific net exports at baseline
    """
    N: int
    sigma: float
    epsilon: float
    theta: np.ndarray
    gamma: np.ndarray
    beta: np.ndarray
    alpha: np.ndarray
    omega: np.ndarray
    delta: np.ndarray
    delta_0: np.ndarray
    L: np.ndarray
    Z: np.ndarray
    P_f: np.ndarray
    NX: Optional[np.ndarray] = None  # To be calibrated in SOEModel

    def __post_init__(self):
        """Validate dimensions. Share parameters are exogenous and not normalized."""
        assert self.theta.shape == (self.N,)
        assert self.gamma.shape == (self.N,)
        assert self.beta.shape == (self.N,)
        assert self.alpha.shape == (self.N,)
        assert self.omega.shape == (self.N, self.N)
        assert self.delta.shape == (self.N, self.N)
        assert self.delta_0.shape == (self.N,)
        assert self.L.shape == (self.N,)
        assert self.Z.shape == (self.N,)
        assert self.P_f.shape == (self.N,)


class SOEModel:
    """
    Solver for the small open economy model following the pseudo-code in Model.md.

    Equilibrium: N prices P_d, N outputs Y_d, and sector-specific net exports NX_i that satisfy:
    1. Price equations: P_d[i] = MC[i] for all i
    2. Quantity equations: Y_d[i] = C_d[i] + sum_k X_kd[i] + NX_i for all i
    3. Price index equation: P_C = 1 (normalization)

    The model automatically solves the non-stochastic equilibrium (Z=1, P_f=1)
    upon initialization to calibrate NX_i such that P_d[i] = 1 and W_i = 1 for all i,
    then NX_i is held fixed across all subsequent equilibria with shocks.

    Interpretation: NX_i represents the net export position of good i, calibrated to
    ensure the baseline equilibrium corresponds between closed and open economy.
    """

    def __init__(self, params: ModelParameters, verbose: bool = False):
        """
        Initialize the model and calibrate sector-specific net exports NX_i at the non-stochastic equilibrium.

        At the non-stochastic equilibrium (Z=1, P_f=1), we target P_d[i] = 1 and W_i = 1 for all i.
        We solve for NX_i from the equilibrium conditions:
        - P_d[i] = MC[i] = 1 (price equations)
        - Y_d[i] = C_d[i] + sum_k X_kd[i] + NX_i (market clearing with net exports)
        - W_i = 1 (wage normalization)

        Args:
            params: Model parameters (must have Z=1 and P_f=1 for all sectors)
            verbose: Print calibration details
        """
        # Verify parameters are at baseline (non-stochastic equilibrium)
        if not (np.allclose(params.Z, 1.0) and np.allclose(params.P_f, 1.0)):
            raise ValueError(
                "SOEModel must be initialized with baseline parameters where Z=1 and P_f=1 "
                "for all sectors. NX_i is calibrated at this baseline and then held fixed "
                "across shocks. To solve with shocks, modify params.Z or params.P_f AFTER "
                "initialization."
            )

        self.p = params
        self.NX = np.zeros(self.p.N)  # Initialize NX_i
        self._calibrating = True  # Flag to indicate we're calibrating NX

        # Solve non-stochastic equilibrium with P_d = 1, W = 1 normalization
        if verbose:
            print("Initializing model and calibrating NX_i at non-stochastic equilibrium...")
            print("Target: P_d[i] = 1, W_i = 1 for all i")

        # Solve baseline equilibrium (NX_i will be computed as residuals)
        eq_baseline = self.solve(verbose=verbose)
        if not eq_baseline['success']:
            raise RuntimeError(f"Failed to solve baseline equilibrium: {eq_baseline['message']}")

        # Calibrate NX_i from market clearing: NX_i = Y_d[i] - C_d[i] - sum_k X_kd[k,i]
        self.NX = eq_baseline['NX']  # Already computed in solve()
        self._calibrating = False

        if verbose:
            wage_income = np.sum(eq_baseline['W'] * self.p.L)
            print(f"\nWage income at baseline: {wage_income:.6f}")
            print(f"\nCalibrated NX: {self.NX}")
            print(f"Consumption price index at baseline: P_C = {eq_baseline['P_C']:.6f}")

    def compute_residuals(self, x: np.ndarray) -> np.ndarray:
        """
        Step 6 of pseudo-code: Compute residuals of 2N equations.

        During calibration (baseline with Z=1, P_f=1):
        - We target P_d[i] = 1 and W_i = 1 for all i
        - N price equations: P_d[i] - 1 = 0
        - N wage equations: W_i - 1 = 0
        - NX_i is backed out from market clearing: NX_i = Y_d[i] - C_d[i] - sum_k X_d[k,i]

        After calibration (with shocks):
        - N price equations: P_d[i] = MC[i]
        - N quantity equations: Y_d[i] = C_d[i] + sum_k X_d[k,i] + NX_i
        - 1 price index equation: P_C = 1
        - NX_i is held fixed at calibrated values

        Args:
            x: stacked vector [P_d (N), Y_d (N)]

        Returns:
            residuals: (2N,) array of residuals
        """
        # Use JIT-compiled function for maximum speed
        NX_val = self.NX if not self._calibrating else np.zeros(self.p.N)
        return compute_residuals(
            x, self.p.N, self.p.P_f, self.p.delta, self.p.delta_0,
            self.p.gamma, self.p.omega, self.p.theta, self.p.alpha,
            self.p.epsilon, self.p.Z, self.p.L, self.p.beta, self.p.sigma,
            NX_val, self._calibrating
        )

    def _create_constraint_function(self):
        """
        Create a JIT-compiled constraint function for use with scipy.optimize.minimize.
        This follows the pattern from eqm_solver_functions.py.
        """
        # Capture all parameters in local variables for the JIT function
        N = self.p.N
        P_f = self.p.P_f
        delta = self.p.delta
        delta_0 = self.p.delta_0
        gamma = self.p.gamma
        omega = self.p.omega
        theta = self.p.theta
        alpha = self.p.alpha
        epsilon = self.p.epsilon
        Z = self.p.Z
        L = self.p.L
        beta = self.p.beta
        sigma = self.p.sigma
        NX_val = self.NX if not self._calibrating else np.zeros(self.p.N)
        calibrating = self._calibrating

        # Create JIT-compiled constraint function with all givens baked in
        constraint_fn = jit(lambda x: compute_residuals(
            x, N, P_f, delta, delta_0, gamma, omega, theta, alpha,
            epsilon, Z, L, beta, sigma, NX_val, calibrating
        ))

        return constraint_fn

    def _analytical_initial_guess(self) -> tuple[np.ndarray, np.ndarray, float]:
        """
        Compute analytical initial guess based on linearized solution.

        This follows the approach from eqm_solver_functions.py:
        - For a closed economy, the equilibrium prices satisfy approximately:
          log(P) ≈ -(I - diag(1-alpha) @ Omega)^(-1) @ log(Z)
        - Outputs are then approximated from the input-output structure

        Returns:
            P_d_init: initial price guess (N,)
            Y_d_init: initial output guess (N,)
            E_init: initial exchange rate guess (scalar)
        """
        N = self.p.N

        # Compute inverse of Leontief matrix
        inv_matrix = np.linalg.inv(np.eye(N) - np.diag(1 - self.p.alpha) @ self.p.omega)

        # Price guess: exp(-(I - diag(1-alpha) @ Omega)^(-1) @ log(Z))
        log_Z = np.log(self.p.Z)
        exp_term = np.exp(-inv_matrix @ log_Z)
        P_d_init = exp_term

        # Output guess: (beta.T @ inv_matrix).T / P
        # This comes from the market clearing condition with consumption ~ beta / P
        Y_d_init = (self.p.beta.T @ inv_matrix).T / exp_term

        # Exchange rate guess
        E_init = 1.0

        return P_d_init, Y_d_init, E_init

    def solve(self, P_d_init: Optional[np.ndarray] = None,
              Y_d_init: Optional[np.ndarray] = None,
              method: str = 'least_squares',
              verbose: bool = False) -> dict:
        """
        Solve for equilibrium using root-finding.

        Args:
            P_d_init: initial guess for prices (default: analytical guess)
            Y_d_init: initial guess for outputs (default: analytical guess)
            method: solver method - 'least_squares' (default) or 'minimize' (SLSQP with constraints)
            verbose: print progress

        Returns:
            Dictionary with equilibrium values
        """
        N = self.p.N

        # Use analytical initial guess if not provided
        if P_d_init is None or Y_d_init is None:
            P_analytical, Y_analytical, E_analytical = self._analytical_initial_guess()
            if P_d_init is None:
                P_d_init = P_analytical
            if Y_d_init is None:
                Y_d_init = Y_analytical
            E0 = [E_analytical]
        else:
            E0 = [1.0]

        x0 = np.concatenate([P_d_init, Y_d_init, E0])

        # Solve system of equations using selected method
        if method == 'least_squares':
            result = least_squares(self.compute_residuals, x0, method='trf', ftol=1e-6, xtol = 1e-6, gtol = 1e-6, max_nfev=10000,
                                   bounds=(1e-10, np.inf), verbose=2 if verbose else 0)
            # check if success
            if not result.success:
                print(f"Least squares solver failed: {result.message}")
        elif method == 'minimize':
            # Trivial objective function (placeholder)
            @jit
            def trivial(x):
                return 1.0

            # Create constraint dictionary with JIT-compiled function
            constraint_fn = self._create_constraint_function()
            constraints = {
                'type': 'eq',
                'fun': constraint_fn
            }

            # Perform the optimization using SLSQP
            result = minimize(
                fun=trivial,
                x0=x0,
                method='SLSQP',
                constraints=constraints,
                options={'disp': verbose, 'maxiter': 1000}
            )

            if not result.success:
                print(f"Minimize solver failed: {result.message}")
                # solve with least squares as fallback
                result = least_squares(self.compute_residuals, x0, method='trf', ftol=1e-6, xtol = 1e-6, gtol = 1e-6, max_nfev=10000,
                                   bounds=(1e-10, np.inf), verbose=2 if verbose else 0)
                if not result.success:
                    print(f"Fallback least squares solver also failed: {result.message}")
        else:
            raise ValueError(f"Unknown method: {method}. Choose 'least_squares' or 'minimize'.")

        # Extract solution
        P_d = result.x[:N]
        Y_d = result.x[N:2*N]
        E = result.x[2*N]

        # Compute all equilibrium quantities
        W = compute_wages(P_d, Y_d, self.p.Z, self.p.epsilon, self.p.alpha, self.p.L)
        MC = compute_marginal_costs(P_d, W, E, self.p.P_f, self.p.delta, self.p.gamma,
                                    self.p.omega, self.p.theta, self.p.alpha, self.p.epsilon, self.p.Z)
        X_d = compute_intermediate_demands(P_d, Y_d, E, self.p.P_f, self.p.delta,
                                            self.p.gamma, self.p.omega, self.p.theta,
                                            self.p.alpha, self.p.epsilon, self.p.Z, self.p.L)
        C_d, P_C = compute_consumption_demands(W, P_d, E, self.p.P_f, self.p.delta_0,
                                               self.p.gamma, self.p.beta, self.p.sigma, self.p.L, self.NX, self._calibrating)
        C = np.sum(W * self.p.L) # + self.NX
        residuals = self.compute_residuals(result.x)

        # Compute NX_i from market clearing (during calibration) or use fixed NX_i (after)
        if self._calibrating:
            NX_computed = Y_d - C_d - np.sum(X_d, axis=0)
        else:
            NX_computed = self.NX.copy()

        # Compute P_C (aggregate consumption price index)
        P_armington, P_C = compute_consumption_price(P_d, E, self.p.P_f, self.p.delta_0,
                                                     self.p.gamma, self.p.beta, self.p.sigma)
        if verbose:
            print(f"Solver success: {result.success}")
            print(f"Max absolute residual: {np.max(np.abs(residuals)):.2e}")
            print(f"Min/Max P_d: {np.min(P_d):.4f} / {np.max(P_d):.4f}")
            print(f"Min/Max Y_d: {np.min(Y_d):.4f} / {np.max(Y_d):.4f}")
            print(f"Min/Max W: {np.min(W):.4f} / {np.max(W):.4f}")
            print(f"P_C: {P_C:.6f}")
            print(f"E: {E:.6f}")

        return {
            'success': result.success,
            'message': result.message,
            'P_d': P_d,
            'E': E,
            'Y_d': Y_d,
            'W': W,
            'MC': MC,
            'X_d': X_d,
            'C_d': C_d,
            'C': C,
            'P_C': P_C,
            'NX': NX_computed,
            'residuals': residuals,
            'max_residual': np.max(np.abs(residuals))
        }


def create_example_parameters(N: int = 3, seed: int = 42) -> ModelParameters:
    """
    Create example parameters for testing.

    Note: This function provides normalized share parameters (beta and omega rows sum to 1)
    as an example, but the model treats all share parameters as exogenous inputs.
    """
    np.random.seed(seed)

    # Elasticities
    sigma = 0.5      # Consumption elasticity across goods
    epsilon = 0.8    # Labor-intermediate elasticity
    theta = np.full(N, 0.5)   # Intermediate input elasticity
    gamma = np.full(N, 3.0)   # Armington elasticity (applies to both firms and households)

    # Share parameters
    beta = np.ones(N) / N  # Equal consumption shares
    alpha = np.full(N, 0.5)  # 50% labor share

    # Input-output matrix
    omega = np.ones((N, N)) / N

    # Armington domestic shares
    delta = np.full((N, N), 1)
    delta_0 = np.full(N, 1)

    # Endowments
    L = np.ones(N) / N
    Z = np.ones(N)

    # Foreign prices
    P_f = np.ones(N)

    return ModelParameters(
        N=N, sigma=sigma, epsilon=epsilon, theta=theta, gamma=gamma,
        beta=beta, alpha=alpha, omega=omega, delta=delta, delta_0=delta_0,
        L=L, Z=Z, P_f=P_f
    )


if __name__ == "__main__":
    # Test with 10 sectors
    print("Creating 10-sector model...")
    params = create_example_parameters(N=66)
    # set first row of delta = 1, rest = .7
    # params.delta = .7 * params.delta

    # Initialize model (automatically solves baseline and calibrates T)
    model = SOEModel(params, verbose=True)

    # Solve a counterfactual with productivity shock
    print("\n" + "="*50)
    print("COUNTERFACTUAL: 10% productivity drop in sector 0")
    print("="*50)
    params.Z[0] = .9
    eq_shocked = model.solve(verbose=True)
    print(f"Counterfactual C: {eq_shocked['C']:.6f}")
    print(f"Counterfactual P_C: {eq_shocked['P_C']:.6f}")
    for i in range(model.p.N):
                print(f"  Sector {i}: Pd_{i} = {eq_shocked['P_d'][i]:8.6f}, MC_{i} = {eq_shocked['MC'][i]:8.6f}")

