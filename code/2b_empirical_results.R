library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(fixest)
library(readxl)
library(RColorBrewer)
library(viridis)
setwd('/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/het_sectoral_prod/V2')

# set baseline year
year = 2024

# load data
load("clean_data/analysis_data.RData")

# load data
est_theta <- read.csv("clean_data/theta_estimates.csv")
est_gamma <- read.csv("clean_data/gamma_estimates.csv")
load("clean_data/code_desc_crosswalk.RData")
non_govt_industries <- unique(code_desc_crosswalk$Code) %>%
  # filter out T codes, G codes, and Trade + Trans + Imports + adjustments
  .[!grepl("^T", .) & !grepl("^G", .) & !grepl("^M", .) &  . != "Trade" & . != "Trans" & . != "SUB" &
      . != "Used" & . != "Other"]
code_desc_crosswalk <- code_desc_crosswalk %>%
  filter(Code %in% non_govt_industries) %>%
  rename(i = Code,
         Industry = `Industry Description`) %>%
  arrange(i) 

est_theta <- est_theta %>%
  left_join(code_desc_crosswalk, by = "i") %>%
  mutate(
    Industry = ifelse(is.na(Industry), "Uniform", Industry)
  ) %>%
  mutate(
    model = factor(model, levels = c("i-theta_j-gamma", "uniform", "commongamma", "nogamma_i-specific", "nogamma_common"),
                   labels = c("j-Armington", "Uniform", "Main", "Biased", "Biased Common")), # use commongamma as main spec
    se_robust_mainonly = ifelse(model == "Main", se_robust, 0)
  ) %>%
  group_by(i) 

est_gamma <- est_gamma %>%
  left_join(code_desc_crosswalk, # by j = i
             by = c("j" = "i")) %>%
  mutate(
    Industry = ifelse(is.na(Industry), "Uniform", Industry)
  ) %>%
  mutate(
    model = factor(model, levels = c("i-theta_j-gamma", "commontheta", "commongamma"),
                   labels = c("j-Armington", "Common Theta", "Main"))
  )

# ============================================================================
# PLOTTING ELASTICITIES
# ============================================================================

# plot theta, model = main and model = nogamma_i-specific
# add +/- se bars
# color main estimates black, biased red

# gen i_lab factor, ordered according to diff
plot_main_data <- est_theta %>%
  filter(model == "Main" | model == "Uniform") %>%
  mutate(
    i_lab = factor(i,
                   levels = est_theta %>%
                     filter(model == "Main") %>%
                     arrange(-theta_i) %>%
                     pull(i))
  )

# two_colors black and red
two_colors <- c("black", "red")
plot_main <- ggplot(plot_main_data, aes(x = as.factor(i_lab), y = theta_i, color = model)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = theta_i - 1.65*se_robust, ymax = theta_i + 1.65*se_robust), width = 1) +
  theme_minimal() +
  labs(title = "", x = "Industry", y = "Elasticity") +
  theme(text = element_text(family = "serif", size = 18),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank()) +
  scale_color_manual(values = c("Main" = two_colors[1], "Uniform" = two_colors[2]), 
                     breaks = c("Main", "Uniform"),
                     labels = c("Main" = "Sector-Specific", "Uniform" = "Uniform")) +
  coord_cartesian(ylim = c(-.5, 3.5)) 

plot_theta_biased_data <- est_theta %>% 
  filter(model == "Main" | model == "Biased") %>%
  group_by(i) %>%
  mutate(
    main_biased_diff = theta_i[model == "Main"] - theta_i[model == "Biased"]
  ) %>%
  ungroup() %>%
  arrange(-main_biased_diff) %>%
  mutate(
    i_lab = factor(i, levels = unique(i))
  ) 

plot_theta_biased <- ggplot(plot_theta_biased_data, aes(x = as.factor(i_lab), y = theta_i, color = model)) +
  geom_point(size = 3) +
  geom_errorbar(data = plot_theta_biased_data %>% filter(model == "Main"),
                aes(ymin = theta_i - 1.65*se_robust, ymax = theta_i + 1.65*se_robust), width = 1) +
  theme_minimal() +
  labs(title = "", x = "Industry", y = "Elasticity") +
  theme(text = element_text(family = "serif", size = 18),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank()) +
  scale_color_manual(values = c("Main" = two_colors[1], "Biased" = two_colors[2]),
                     labels = c("Main", "Biased")) +
  coord_cartesian(ylim = c(-.5, 3.5))
  
# Save plots
ggsave("figures/theta_i_main.pdf", plot = plot_main, width = 8, height = 6)
ggsave("figures/theta_i_biased.pdf", plot = plot_theta_biased, width = 8, height = 6)
  
# write Latex long table
# Create table with Industry, Estimate (Main), SE, Biased Estimate, Biased SE
theta_table_data <- est_theta %>%
  filter(model %in% c("Main", "Biased")) %>%
  select(i, Industry, model, theta_i, se_robust) %>%
  pivot_wider(
    names_from = model,
    values_from = c(theta_i, se_robust),
    names_glue = "{model}_{.value}"
  ) %>%
  arrange(Main_theta_i)

# Get Uniform theta estimate and Uniform Armington (gamma) estimate
uniform_est <- est_theta %>% filter(model == "Uniform") %>%
  ungroup() %>% slice(1) %>% select(theta_i, se_robust)
armington_est <- est_gamma %>% filter(model == "Uniform") %>%
  ungroup() %>% slice(1) %>% select(gamma_j, se_robust)

# Build LaTeX longtable
latex_lines <- c(
  "\\begin{longtable}{llcccc}",
  "\\caption{Elasticity Estimates by Industry} \\label{tab:theta_estimates} \\\\",
  "\\toprule",
  "Industry & Code & Est. & SE & Biased Est. & Biased SE \\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\multicolumn{6}{c}%",
  "{{\\bfseries \\tablename\\ \\thetable{} -- continued from previous page}} \\\\",
  "\\toprule",
  "Industry & Code & Est. & SE & Biased Est. & Biased SE \\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule \\multicolumn{6}{r}{{Continued on next page}} \\\\",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot"
)

# Add industry rows
for (row in 1:nrow(theta_table_data)) {
  industry_name <- theta_table_data$Industry[row]
  industry_code <- theta_table_data$i[row]
  # Truncate at 30 characters with ... if needed
  if (nchar(industry_name) > 50) {
    industry_name <- paste0(substr(industry_name, 1, 47), "...")
  }
  industry_name <- gsub("&", "\\\\&", industry_name)
  main_theta <- sprintf("%.3f", theta_table_data$Main_theta_i[row])
  main_se <- sprintf("%.3f", theta_table_data$Main_se_robust[row])
  biased_theta <- sprintf("%.3f", theta_table_data$Biased_theta_i[row])
  biased_se <- sprintf("%.3f", theta_table_data$Biased_se_robust[row])
  latex_lines <- c(latex_lines, paste0(industry_name, " & ", industry_code, " & ", main_theta, " & ", main_se, " & ", biased_theta, " & ", biased_se, " \\\\"))
}

# Add Uniform and Armington rows
latex_lines <- c(latex_lines, "\\midrule")
latex_lines <- c(latex_lines, paste0("Uniform & --- & ", sprintf("%.3f", uniform_est$theta_i), " & ", sprintf("%.3f", uniform_est$se_robust), " & --- & --- \\\\"))
latex_lines <- c(latex_lines, paste0("Armington & --- & ", sprintf("%.3f", armington_est$gamma_j), " & ", sprintf("%.3f", armington_est$se_robust), " & --- & --- \\\\"))
latex_lines <- c(latex_lines, "\\end{longtable}")

# Write to file
writeLines(latex_lines, "tables/elasticity_estimates_table.tex")


# ============================================================================
# SAVE ELASTICITIES + EXPENDITURE SHARES FOR QUANTITATIVE EXERCISES
# ============================================================================

# CLEAN TFP
# place-holder; just load it
load("clean_data/industry_tfp.RData")

# CLEAN ELASTICITIES

elasticities <- code_desc_crosswalk %>%
  left_join(
    est_theta %>%
      filter(model == "Main") %>%
      select(i, theta_i),
    by = "i"
  ) %>%
  left_join(
    est_gamma %>%
      filter(model == "j-Armington") %>%
      select(j, gamma_j) %>%
      rename(i = j),
    by = "i"
  ) %>%
  mutate(
    theta_i = ifelse(theta_i == 0, 1e-6, theta_i),
    gamma_j = ifelse(gamma_j == 0, 1e-6, gamma_j),
    gamma_j = ifelse(is.na(gamma_j), 1e-6, gamma_j),
    theta_uniform = est_theta %>% filter(model == "Uniform") %>% distinct() %>% pull(theta_i),
    gamma_uniform = est_gamma %>% filter(model == "Main") %>% distinct() %>% pull(gamma_j)
  ) %>%
  select(i, Industry, theta_i, gamma_j, theta_uniform, gamma_uniform)

# CLEAN CALIBRATION MATRICES
Omega <- analysis_data %>%
  filter(t == year) %>%
  select(i, j, a_ijt) %>%
  filter(i %in% non_govt_industries & j %in% non_govt_industries) %>%
  pivot_wider(names_from = j, values_from = a_ijt, values_fill = 0) 

Omega_mat <- as.matrix(Omega[,-1])  # remove i column for matrix
rownames(Omega_mat) <- Omega$i

Delta <- analysis_data %>%
  filter(t == year) %>%
  # iXj d_ijt shares
  # should be 66 by 66, fill missing values with 0
  select(i, j, s_ijdt) %>%
  filter(i %in% non_govt_industries & j %in% non_govt_industries) %>%
  pivot_wider(names_from = j, values_from = s_ijdt, values_fill = 0) %>%
  arrange(i)

Delta_mat <- as.matrix(Delta[,-1])  # remove i column for matrix
rownames(Delta_mat) <- Delta$i

# regression on HH exppenditure data to get sigma
sigma_reg_data <- analysis_data %>%
  filter(i == j) %>%
  ungroup() %>%
  select(i, t, beta_it, log_p_jdt) %>%
  group_by(i) %>%
  mutate(
    mean_beta_i = mean(beta_it)
  ) %>%
  ungroup() %>%
  mutate(
    rank = dense_rank(desc(mean_beta_i))
  ) %>%
  group_by(i) %>%
  arrange(i, t) %>%
  mutate(
    delta_log_p_jdt = log_p_jdt - lag(log_p_jdt),
    delta_log_beta_it = log(beta_it) - lag(log(beta_it))
  )

sigma_reg <- feols(delta_log_beta_it ~ delta_log_p_jdt| t, data = sigma_reg_data)
sigma <- 1 - coef(sigma_reg)["delta_log_p_jdt"]
  
beta <- analysis_data %>%
  filter(t == year) %>%
  ungroup() %>%
  select(i, beta_it) %>%
  filter(i %in% non_govt_industries) %>%
  arrange(i) %>%
  distinct()

alpha <- analysis_data %>%
  filter(t == year) %>%
  ungroup() %>%
  select(i, alpha_it) %>%
  filter(i %in% non_govt_industries) %>%
  arrange(i) %>%
  distinct()

calibration_vectors <- elasticities %>%
  left_join(alpha, by = "i") %>%
  left_join(beta, by = "i") %>%
  mutate(
    sigma = sigma
  )
  

save(calibration_vectors, Omega_mat, Delta_mat, industry_TFP, file = "clean_data/calibration_data.RData")
# Write csv of elasticities to main folder
substitution_elasticities <- calibration_vectors %>%
  select(Industry_Code = i, Industry_Desc = Industry, theta_i, theta_uniform, armington = gamma_uniform, household = sigma)
write.csv(substitution_elasticities, "elasticity_estimates.csv", row.names = FALSE)
