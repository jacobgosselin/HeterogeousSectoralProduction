library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(fixest)
library(texreg)
library(readxl)
library(RColorBrewer)
library(viridis)
setwd('/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/het_sectoral_prod/V2')

# set baseline year
year = 2024

# load data
load("clean_data/analysis_data.RData")
load("clean_data/code_desc_crosswalk.RData")
load("clean_data/atalay_data.RData")

non_govt_industries <- unique(code_desc_crosswalk$Code) %>%
  # filter out T codes, G codes, and Trade + Trans + Imports + adjustments
  .[!grepl("^T", .) & !grepl("^G", .) & !grepl("^M", .) &  . != "Trade" & . != "Trans" & . != "SUB" &
      . != "Used" & . != "Other"]
code_desc_crosswalk <- code_desc_crosswalk %>%
  filter(Code %in% non_govt_industries) %>%
  rename(i = Code,
         Industry = `Industry Description`) %>%
  arrange(i) 

# ============================================================================
# DESCRIPTIVE PLOTS
# ============================================================================

# Function to create Omega heatmap for a given year
create_omega_heatmap <- function(year_val, save_path = "figures/omega_heatmap/") {

  # Prepare data for heatmap
  omega_heatmap_data <- analysis_data %>%
    filter(t == year_val) %>%
    select(i, j, a_ijt) %>%
    filter(i %in% non_govt_industries & j %in% non_govt_industries)
  
  # ensure all i j combinations present; fill missing with 0
  all_combinations <- expand_grid(
    i = non_govt_industries,
    j = non_govt_industries
  )
  
  # Join with your data
  omega_heatmap_data <- all_combinations %>%
    left_join(omega_heatmap_data, by = c("i", "j")) %>%
    mutate(a_ijt = replace_na(a_ijt, 0))
  
  # Create heatmap
  omega_plot <- ggplot(omega_heatmap_data, aes(x = j, y = i, fill = log(a_ijt))) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", name = "Expenditure\nShare") +
    theme_minimal() +
    labs(title = paste0(year_val),
         x = "Supplier Industry (j)",
         y = "Purchasing Industry (i)") +
    theme(text = element_text(family = "serif", size = 24),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.position = "none") 

  # Save heatmap
  filename <- paste0(save_path, "omega_heatmap_", year_val, ".pdf")
  ggsave(filename, plot = omega_plot, width = 10, height = 10)

  return(omega_plot)
}

# Generate heatmaps for all years (2000-2024)
all_years <- 1997:2024
for (yr in all_years) {
  create_omega_heatmap(yr)
}


# --- 3-PANEL VARIANCE PLOT: Expenditure Shares, Prices, Import Ratios ---

# Panel A: Variance in delta expenditure shares (matrix: i x j) 

# Generate heatmap of variance in delta_a_ijdt over time (i.e. by industry pair)
omega_variance_data <- analysis_data %>%
  group_by(i, j) %>%
  summarize(var_delta_a_ijdt = var(delta_a_ijdt, na.rm = TRUE))
# ensure all i j combinations present; fill missing with 0
all_combinations <- expand_grid(
  i = non_govt_industries,
  j = non_govt_industries
) 
# Join with your data
omega_variance_data <- all_combinations %>%
  left_join(omega_variance_data, by = c("i", "j")) %>%
  mutate(var_delta_a_ijdt = replace_na(var_delta_a_ijdt, 0))

# Panel B: Variance in delta prices (vector: j) 
price_variance_data <- analysis_data %>%
  filter(j %in% non_govt_industries) %>%
  group_by(j) %>%
  summarize(var_delta_p_jdt = var(delta_p_jdt, na.rm = TRUE)) %>%
  ungroup()

# Ensure all j present
price_variance_data <- tibble(j = non_govt_industries) %>%
  left_join(price_variance_data, by = "j") %>%
  mutate(var_delta_p_jdt = replace_na(var_delta_p_jdt, 0))

# Panel C: Variance in delta import ratios (vector: j) 
import_variance_data <- analysis_data %>%
  filter(j %in% non_govt_industries) %>%
  group_by(j) %>%
  summarize(var_delta_s_jdt = var(delta_s_ijdt, na.rm = TRUE)) %>%
  ungroup()

# Ensure all j present
import_variance_data <- tibble(j = non_govt_industries) %>%
  left_join(import_variance_data, by = "j") %>%
  mutate(var_delta_s_jdt = replace_na(var_delta_s_jdt, 0))

# Create individual plots for 3-panel figure 

# Compute common scale limits across all three panels
all_log_variances <- c(
  log(omega_variance_data$var_delta_a_ijdt + 1e-6),
  log(price_variance_data$var_delta_p_jdt + 1e-6),
  log(import_variance_data$var_delta_s_jdt + 1e-6)
)
common_limits <- range(all_log_variances, na.rm = TRUE)

# Common theme for panels
panel_theme <- theme_minimal() +
  theme(text = element_text(family = "serif", size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 24),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0.5))

# Panel A: Expenditure shares variance (matrix heatmap)
panel_a <- ggplot(omega_variance_data, aes(x = j, y = i, fill = log(var_delta_a_ijdt + 1e-6))) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "Log Variance", limits = common_limits) +
  panel_theme +
  labs(title = expression("Expenditure Shares ("*Delta*log*Omega[ijt]*")"),
       x = "Supplier Industry (j)",
       y = "Purchasing Industry (i)") 

# Panel B: Price variance (vector heatmap - single column)
panel_b <- ggplot(price_variance_data, aes(x = 1, y = j, fill = log(var_delta_p_jdt + 1e-6))) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "Log Variance", limits = common_limits) +
  panel_theme +
  labs(title = expression("Prices ("*Delta*log*P[jt]*")"),
       x = "",
       y = "Industry (j)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Panel C: Import ratio variance (vector heatmap - single column)
panel_c <- ggplot(import_variance_data, aes(x = 1, y = j, fill = log(var_delta_s_jdt + 1e-6))) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "Log Variance", limits = common_limits) +
  panel_theme +
  labs(title = expression("Imports ("*Delta*log*Phi[jt]*")"),
       x = "",
       y = "Industry (j)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Combine into 3-panel figure using patchwork
library(patchwork)

# Layout: 3 panels horizontally with common legend
variance_3panel <- (panel_a | panel_b | panel_c) +
  plot_layout(widths = c(4, 1, 1), guides = "collect") &
  theme(legend.position = "bottom")

# Save 3-panel figure
ggsave("figures/variance_3panel.pdf", plot = variance_3panel, width = 16, height = 8)

# ============================================================================
# ATALAY IV REG COMPARISON
# ============================================================================

# only include when we have all needed variables for atalay iv
atalay_data <- atalay_data %>%
  filter(!is.na(delta_a_ijdt) & !is.na(rel_price_change) & !is.na(atalay_iv_eq14) & !is.na(delta_p_jdt))

# Atalay IV regression
# note that I use eq 14 and 16 from Atalay, but not 15; this is because I only want to IV for II price index
# and the input price index. Eq 15 is only plausibly relevant for the output price index.
atalay_reg <- feols(
  delta_a_ijdt ~ 1 | rel_price_change ~ atalay_iv_eq14 + atalay_iv_eq16,
  data = atalay_data
)
closed_economy_comp <- feols(
  delta_a_ijdt ~ delta_p_jdt | i + t,
  data = atalay_data
)

atalay_reg_sum <- summary(atalay_reg, vcov = "hetero")
closed_economy_comp_sum <- summary(closed_economy_comp, vcov = "hetero")

# Export texreg table with "Elasticity" = 1 - coefficient
atalay_coef <- coef(atalay_reg_sum)["fit_rel_price_change"]
atalay_se <- sqrt(vcov(atalay_reg_sum)["fit_rel_price_change", "fit_rel_price_change"])
closed_coef <- coef(closed_economy_comp_sum)["delta_p_jdt"]
closed_se <- sqrt(vcov(closed_economy_comp_sum)["delta_p_jdt", "delta_p_jdt"])

atalay_tr <- createTexreg(
  coef.names = "Elasticity",
  coef = 1 - atalay_coef,
  se = atalay_se,
  pvalues = 2 * pnorm(-abs((1 - atalay_coef) / atalay_se)),
  gof.names = c("Num. obs."),
  gof = c(nobs(atalay_reg_sum)),
  gof.decimal = c(FALSE)
)

closed_tr <- createTexreg(
  coef.names = "Elasticity",
  coef = 1 - closed_coef,
  se = closed_se,
  pvalues = 2 * pnorm(-abs((1 - closed_coef) / closed_se)),
  gof.names = c("Num. obs."),
  gof = c(nobs(closed_economy_comp_sum)),
  gof.decimal = c(FALSE)
)

# Add GMM estimate
est_theta <- read.csv("clean_data/theta_estimates.csv")
theta_common <- est_theta %>%
  filter(model == "uniform") %>%
  select(theta_i, se_robust)

uniform_tr <- createTexreg(
  coef.names = "Elasticity",
  coef = theta_common$theta_i,
  se = theta_common$se_robust,
  pvalues = 2 * pnorm(-abs(theta_common$theta_i / theta_common$se_robust)),
  gof.names = c("Num. obs."),
  gof = c(nobs(closed_economy_comp_sum)),
  gof.decimal = c(FALSE)
)

texreg(
  list(atalay_tr, closed_tr, uniform_tr),
  custom.model.names = c("Atalay IV", "Uniform (Closed Economy)", "Uniform"),
  file = "tables/atalay_comparison.tex",
  stars = NULL,
  table = FALSE
)

