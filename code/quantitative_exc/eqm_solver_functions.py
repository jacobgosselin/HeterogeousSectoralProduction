# Import libraries
from numba import jit
import numpy as np
from scipy.stats import multivariate_normal
from scipy.optimize import minimize

####################################################################################################
# Functions defining equilibrium constraints
####################################################################################################

@jit
def xij(A, Omega, alpha, epsilon, theta, Q, Y, P):
    N = len(alpha)
    x_ij = np.zeros((N, N))
    for i in range(N):
        for j in range(N):
            x_ij[i, j] = Y[i] * (1 - alpha[i]) * Omega[i, j] * P[i]**epsilon * P[j]**(-theta[i]) * Q[i]**(theta[i]-epsilon) * A[i]**(epsilon-1)
    return x_ij

@jit
def mc_i(A, alpha, epsilon, Q, W):
    N = len(alpha)
    mc = np.zeros(N)
    for i in range(N):
        mc[i] = A[i]**(-1) * (alpha[i] * W[i]**(1-epsilon) + (1-alpha[i])*Q[i]**(1-epsilon))**(1/(1-epsilon))    
    return mc

@jit
def unit_cost(Omega, p, theta):
    N = len(theta)
    q = np.zeros(N)
    for i in range(N):
        p_exp = p**(1-theta[i])
        q[i] = (Omega[i,:] @ p_exp)**(1/(1-theta[i]))
    return q

@jit
def multisector_constraints_norealloc(X, A, beta, Omega, alpha, epsilon, theta, sigma, L):
    N = len(alpha)
    p = X[:N]
    y = X[N:2*N]

    q = unit_cost(Omega, p, theta)
    w = p * (A**((epsilon-1)/epsilon)) * (alpha**(1/epsilon)) * (y**(1/epsilon)) * (1/L)**(1/epsilon)
    C = w@L
    x_ij = xij(A, Omega, alpha, epsilon, theta, q, y, p)
    mc = mc_i(A, alpha, epsilon, q, w)

    Out = np.zeros(2*N)
    for i in range(N):
        Out[i] = p[i] - mc[i]
    for i in range(N):
        input_use = sum(x_ij[:,i])
        c_i = beta[i] * p[i]**(-sigma) * C
        Out[N+i] = y[i] - input_use - c_i
    return Out

@jit
def multisector_constraints_realloc(X, A, beta, Omega, alpha, epsilon, theta, sigma, L):
    N = len(alpha)
    p = X[:N]
    y = X[N:2*N]

    w = np.ones(N) # constant wage; normalize to 1, as numeraire 
    C = (beta@(p**(1-sigma)))**(1/(sigma-1))
    q = unit_cost(Omega, p, theta)
    x_ij = xij(A, Omega, alpha, epsilon, theta, q, y, p)
    mc = mc_i(A, alpha, epsilon, q, w)

    Out = np.zeros(2*N)
    for i in range(N):
        Out[i] = p[i] - mc[i]
    for i in range(N):
        input_use = sum(x_ij[:,i])
        c_i = beta[i] * p[i]**(-sigma) * C
        Out[N+i] = y[i] - input_use - c_i
    return Out

####################################################################################################
# Functions for solving equilibrium
####################################################################################################

@jit
def trivial(X):
    # Define the objective function here
    # This is a placeholder example
    return 1 

def eqm_solver(A, beta, Omega, alpha, epsilon, theta, sigma, L, P_guess, Y_guess, reallocate=False, return_L = False):

    # Define the initial guess
    init = np.concatenate((P_guess, Y_guess))

    # smarter initial guess
    N = len(alpha)
    inv_matrix = np.linalg.inv(np.eye(N) - np.diag(1 - alpha) @ Omega)
    log_A = np.log(A)
    exp_term = np.exp(-inv_matrix @ log_A)
    init = np.concatenate((exp_term, (beta.T @ inv_matrix).T / exp_term))

    # Define the constraint dictionary (with and without reallocation)
    if reallocate:
        constraints = {
            'type': 'eq',
            'fun': jit(lambda X: multisector_constraints_realloc(X, A, beta, Omega, alpha, epsilon, theta, sigma, L))
        }
    else:
        constraints = {
            'type': 'eq',
            'fun': jit(lambda X: multisector_constraints_norealloc(X, A, beta, Omega, alpha, epsilon, theta, sigma, L))
        }

    # Perform the optimization
    result = minimize(
        fun=trivial,
        x0=init,
        method='SLSQP',
        constraints=constraints,
        options={'disp': False, 'maxiter': 250}
    )

    # Extract the solution and exit flag
    Soln = result.x
    exitfl = result.success
    # if no success in optimization, print "NO CONVERGENCE"
    if not exitfl:
        print("NO CONVERGENCE")
        return None, None
    else:
        print("Convergence!")

    # Recover GDP
    p_eqm = Soln[:len(alpha)]
    y_eqm = Soln[len(alpha):2*len(alpha)]
    w_eqm = p_eqm * (A**((epsilon-1)/epsilon)) * (alpha**(1/epsilon)) * (y_eqm**(1/epsilon)) * (1/L)**(1/epsilon)
    if reallocate:
        C_eqm = (beta@(p_eqm**(1-sigma)))**(1/(sigma-1))
        L_eqm = (p_eqm * (A**((epsilon-1)/epsilon)) * (alpha**(1/epsilon)) * (y_eqm**(1/epsilon)))**(epsilon)
    else:
        w_eqm = p_eqm * (A**((epsilon-1)/epsilon)) * (alpha**(1/epsilon)) * (y_eqm**(1/epsilon)) * (1/L)**(1/epsilon)
        C_eqm = w_eqm@L
    if return_L:
        return L_eqm 
    else: 
        return C_eqm, y_eqm
    
####################################################################################################
# Functions for simulating equilibrium with TFP shocks
####################################################################################################

def draw_multivariate_normal(cov_matrix, num_samples, seed=None):
    rng = np.random.default_rng(seed)
    mean_vector = np.zeros(cov_matrix.shape[0]) # Mean vector (zero mean)
    samples = multivariate_normal.rvs(mean=mean_vector, cov=(cov_matrix), size=num_samples, random_state=rng) # Generate samples from the multivariate normal distribution using the RNG
    return samples

def eqm_simulator(beta, Omega, alpha, epsilon, theta, sigma, L, P_guess, Y_guess, samples):

    num_samples = len(samples)
    gdp_dist = np.zeros(num_samples)
    sectoral_output = np.zeros((num_samples, len(alpha)))

    # Loop over the samples
    for i in range(num_samples):
        A_rand = np.exp(samples[i])
        gdp_dist[i], sectoral_output[i] = eqm_solver(A_rand, beta, Omega, alpha, epsilon, theta, sigma, L, P_guess, Y_guess) # Solve for the equilibrium
        print("Remaining samples: ", num_samples - i - 1)
    
    return gdp_dist, sectoral_output

def CD_simulator(domar_weights, samples):
    num_samples = len(samples)
    gdp_dist = np.zeros(num_samples)
    for i in range(num_samples):
        A_rand = np.exp(samples[i])
        gdp_dist[i] = np.exp(domar_weights@np.log(A_rand))
    return gdp_dist