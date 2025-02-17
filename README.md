# Heterogeneity in Sectoral Production and the Macro Effect of Sectoral Shocks: Replication Code

Replication code for "Heterogeneity in Sectoral Production and the Macro Effect of Sectoral Shocks" ([arXiv](https://arxiv.org/abs/2502.07896). Generates data for empirical analysis, empirical results, and quantitative results. For a full run from raw data to tables and figures used in the paper, run..

1. code/master_run_1997-2023.R, which generates data/cleaned/resid_data_1998_2023.RData (and components of it), the main dataset used for empirical results.
2. code/analysis_1.Rmd, which generates all empirical results in paper.
3. code/quantitative_exc/quantitative_exercises.ipynb, which generates all quantitative results in paper.
4. code/analysis_2.Rmd, which generates figures summarizing the quantitative results.

The paper's appendix contains empirical results using an extended dataset stretching back to 1947. To generate these results, run...

1. code/master_run_1947-1996.R, which generates data/cleaned/resid_data_1964_1996.RData (and components of it).
2. code/analysis_hist.Rmd, which generates all empirical results using the extended dataset in the paper appendix.

Note that the raw components used to generate patent_data_agg.RData are missing, as they include Compustat data.
