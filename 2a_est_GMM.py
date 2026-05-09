import os

import pandas as pd
from helper_fncts.GMM_Est_Fncts import *

with open('output_directory.txt') as f:
    out_dir = f.read().strip()

# load clean_data/analysis_data_filtered.csv
analysis_data = pd.read_csv(os.path.join(out_dir, 'clean_data/analysis_data_filtered.csv'))

df = pd.DataFrame(
    {
        'i': analysis_data['i'],
        'j': analysis_data['j'],
        't': analysis_data['t'],
        'y_ijt': analysis_data['delta_a_ijdt'],
        'p_jt': analysis_data['delta_p_jdt'],
        's_ijt': analysis_data['delta_s_ijdt'],
        'tradeable': analysis_data['tradeable']
    }
)

theta_i, gamma_j = estimate_elasticities_gmm(df, gamma_type="j-specific", theta_type="i-specific")
commongamma_theta_i, commongamma_gamma = estimate_elasticities_gmm(df, gamma_type="common", theta_type="i-specific")
commontheta_theta, commontheta_gamma = estimate_elasticities_gmm(df, gamma_type="common", theta_type="common")
nogamma_theta_i, nogamma_gamma = estimate_elasticities_gmm(df, gamma_type="none", theta_type="i-specific")
nogamma_theta, nogamma_gamma = estimate_elasticities_gmm(df, gamma_type="none", theta_type="common")

# combine theta results into single df
theta_results = pd.concat([
    theta_i.assign(model='i-theta_j-gamma'),
    commontheta_theta.assign(model='uniform'),
    commongamma_theta_i.assign(model='commongamma'),
    nogamma_theta_i.assign(model='nogamma_i-specific'),
    nogamma_theta.assign(model='nogamma_common')
])

gamma_results = pd.concat([
    gamma_j.assign(model='i-theta_j-gamma'),
    commontheta_gamma.assign(model='commontheta'),
    commongamma_gamma.assign(model='commongamma')
])

theta_results.to_csv(os.path.join(out_dir, 'clean_data/theta_estimates.csv'), index=False)
gamma_results.to_csv(os.path.join(out_dir, 'clean_data/gamma_estimates.csv'), index=False)
