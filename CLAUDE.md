# CLAUDE.md — HeterogeousSectoralProduction

## Project Overview

Replication package for the paper **"Sector-Specific Substitution and the Effect of Sectoral Shocks"** by Jacob Toner Gosselin (Northwestern University). The project estimates sector-specific intermediate input substitution elasticities for 66 US industries and studies how those elasticities shape the propagation of sectoral shocks in a multi-sector GE model.

## Repository Structure

```
/                           ← repo root (always run scripts from here)
├── output_directory.txt    ← single-line path to the external output folder
├── elasticity_estimates.csv ← main empirical output, versioned in repo
├── helper_fncts/
│   ├── GMM_Est_Fncts.py    ← GMM moment conditions, weighting, optimization
│   ├── soe_model.py        ← small-open-economy model (production, cost min, equilibrium)
│   └── soe_solver_fncts.py ← equilibrium solver and shock simulation routines
├── 0_api_pull.R            ← BEA API pull (requires api_keys.txt)
├── 1a_clean_main.R         ← BEA supply/use tables → intermediate input shares, PCE, etc.
├── 1b_clean_supplemental.R ← BLS KLEMS, FRED, concordances → analysis_data.csv
├── 2a_est_GMM.py           ← GMM estimation → theta_estimates.csv, gamma_estimates.csv
├── 2b_empirical_results.R  ← figures and tables from estimation results
├── 3a_solve_shocks.py      ← GE model solutions for productivity/price/calibrated shocks
├── 3b_quant_results.py     ← open-economy tables/figures
├── 3c_quant_results_closed.py ← closed-economy tables/figures
├── 4_MISC_empirical_results.R ← robustness checks (Atalay comparison, etc.)
└── 4_MISC_stylized_ex.py   ← stylized model illustrations
```

## Output Directory

`output_directory.txt` points to:
```
/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/het_sectoral_prod/V2
```

Subdirectories used by scripts:
- `raw_data/` — BEA API output
- `clean_data/` — processed datasets (`.RData`, `.csv`)
- `figures/` and `figures/omega_heatmap/` — PDF figures
- `tables/` — `.tex` table files
- `paper/` — `draft.tex`, `draft.pdf`, `UF_FRED_paper_style.sty`, `paper_stats.csv`

## Variable Notation (Code ≠ Paper)

| Code variable | Paper notation | Meaning |
|---|---|---|
| `a_ijdt` | $\Omega_{ijt}$ | Intermediate expenditure share of sector $i$ on sector $j$ |
| `s_ijdt` | $\Phi_{ijt}$ | Import ratio (domestic share of $i$'s purchases of $j$) |
| `p_jdt` | $P_{jt}$ | Domestic price of sector $j$ output |
| `theta_i` | $\theta_i$ | Sector-specific intermediate input substitution elasticity |
| `xi` / `gamma` | $\xi$ | Armington elasticity (domestic/foreign substitution) |

## Model Summary

Static small-open-economy GE model, $N = 66$ sectors.

- **Firms**: 3-nest CES production — labor + CES aggregate of intermediate inputs, each intermediate being a CES bundle of domestic and imported varieties.
- **Household**: CES consumption aggregator across domestic sectoral goods.
- **Trade**: Exogenous foreign prices; exports via isoelastic foreign demand schedule.
- **Calibration**: Base year 2024 normalized to equilibrium. Share parameters equal base-year expenditure shares. Export demand shifters backed out from market clearing.

Key elasticities:
- $\theta_i$: sector-specific intermediate input substitution (central contribution)
- $\xi$: Armington elasticity (common)
- $\nu$: household consumption elasticity

## Estimation

GMM on year-over-year changes in spending shares and prices:
$$\Delta \log \Omega_{ijt} = (1-\theta_i)\Delta \log P_{jt} + \frac{\xi - \theta_i}{\xi-1}\Delta \log \Phi_{ijt} + \eta_{it} + \epsilon_{ijt}$$

Multiple model variants estimated in `2a_est_GMM.py`: sector-specific $\theta_i$ with sector-specific $\gamma_j$, common $\gamma$, no $\gamma$, and uniform $\theta$.

## Running the Pipeline

Always run from the repo root:
```
0_api_pull.R → 1a_clean_main.R → 1b_clean_supplemental.R →
2a_est_GMM.py → 2b_empirical_results.R →
3a_solve_shocks.py → 3b_quant_results.py / 3c_quant_results_closed.py
```

`3a_solve_shocks.py` has toggle flags at the top (`RUN_SEVERE_DOMESTIC`, `RUN_SEVERE_FOREIGN`, `RUN_CALIBRATED`) to control which shock types to run.

## Key Output Files

| File | Description |
|---|---|
| `clean_data/theta_estimates.csv` | All theta model variants |
| `clean_data/gamma_estimates.csv` | Armington elasticity estimates |
| `clean_data/analysis_data.csv` | Main panel dataset |
| `clean_data/calibration_data.RData` | Model calibration matrices (Omega, Delta, etc.) |
| `clean_data/sectoral_outputs_main.csv` | GE shock responses, main spec |
| `figures/theta_i_main.pdf` | Main elasticity estimates figure |
| `figures/variance_3panel.pdf` | Variance decomposition figure |
| `tables/elasticity_estimates_table.tex` | Main results table |
| `paper/draft.tex` | Paper source |
| `elasticity_estimates.csv` | Versioned in repo — main empirical contribution |

## Dependencies

- **R**: BEA API access, data cleaning, figure/table generation
- **Python**: GMM estimation, GE model solving, quantitative results
- **rpy2**: `3a_solve_shocks.py` loads `.RData` calibration files via rpy2
- **BEA API key**: required for `0_api_pull.R` (path set in script)
