# Replication Package: Sector-Specific Substitution and the Effect of Sectoral Shocks

**Author:** Jacob Toner Gosselin (Northwestern University)

This repository contains the code to replicate the results in "Sector-Specific Substitution and the Effect of Sectoral Shocks." 

All generated outputs (raw data, cleaned data, figures, tables) are written to the directory specified in `output_directory.txt`. The only exception is `elasticity_estimates.csv`, which is stored in this repository since it's the main empirical contribution of the paper and I want to encourage folks to use it in their own work! I keep the data separate since it borders on being too large for GitHub; ever since agentic AI I've been keeping all code on GitHub to ensure version control (should've done this sooner!) and store all outputs/write papers in a separate folder.

To run this locally, set the output_directory to a local folder that contains `raw_data/`, `clean_data/`, `figures/`, `figures/omega_heatmap/`, and `tables/` subdirectories. 


## Main Output

- **`elasticity_estimates.csv`** — Sector-specific intermediate input substitution elasticity estimates for 66 US industries, along with the uniform (common) elasticity, Armington elasticity, and household elasticity. This is the central contribution of the paper.

## Code

Scripts are numbered in the order they should be run. Comments within each file describe what they do in detail.

| Script | Language | Description |
|--------|----------|-------------|
| `0_api_pull.R` | R | Pulls raw data from the BEA API (Input-Output Accounts, GDP by Industry, NIPA, price indices). Requires a BEA API key. |
| `1a_clean_main.R` | R | Cleans BEA supply and use tables. Constructs intermediate input shares, PCE, labor compensation, and defense spending by commodity for 66 industries. |
| `1b_clean_supplemental.R` | R | Integrates supplemental data: BLS KLEMS TFP, FRED defense spending, BEA-NAICS concordance, import/domestic price indices. Produces the main analysis dataset. |
| `2a_est_GMM.py` | Python | Estimates sector-specific intermediate input substitution elasticities and the Armington elasticity via GMM. Outputs `theta_estimates.csv` and `gamma_estimates.csv` to `clean_data/`. |
| `2b_empirical_results.R` | R | Processes estimation results. Generates figures (e.g., `theta_i_main.pdf`, `variance_3panel.pdf`) and tables (e.g., `elasticity_estimates_table.tex`) for the paper. |
| `3a_solve_shocks.py` | Python | Solves the multi-sector GE model for equilibrium responses to severe productivity shocks, foreign price shocks, and calibrated sectoral business cycles (1000 draws). Saves results to `clean_data/`. |
| `3b_quant_results.py` | Python | Reads shock results and produces tables and figures for the open-economy calibrations (main and common-theta). |
| `3c_quant_results_closed.py` | Python | Same as `3b` but for the closed-economy calibrations. |
| `4_MISC_empirical_results.R` | R | Additional empirical results and robustness checks (e.g., Atalay comparison). |
| `4_MISC_stylized_ex.ipynb` | Python | Stylized examples illustrating model mechanics. |

### Helper Functions (`helper_fncts/`)

| File | Description |
|------|-------------|
| `GMM_Est_Fncts.py` | GMM estimation functions: moment conditions, weighting matrices, optimization. |
| `soe_model.py` | Small open economy model specification: production functions, cost minimization, equilibrium conditions. |
| `soe_solver_fncts.py` | Solver routines for computing equilibria and simulating shocks. |

## Replication Instructions

1. Edit `output_directory.txt` at the repo root to point to your local output folder (must contain `raw_data/`, `clean_data/`, `figures/`, `figures/omega_heatmap/`, and `tables/` subdirectories).
2. Obtain a BEA API key and update the path in `0_api_pull.R` (the `api_keys.txt` path).
3. Run scripts in numerical order from the **repo root**: `0_api_pull.R` → `1a` → `1b` → `2a` → `2b` → `3a` → `3b` / `3c`.
4. All scripts read `output_directory.txt` at startup to resolve input/output paths.
