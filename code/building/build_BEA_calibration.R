# libraries
library(dplyr)
library(tidyr)
library(fixest)
library(zoo)
library(purrr)

# load data
load("data/cleaned/code_desc_crosswalk.RData")
load("data/cleaned/A_Int_byyear_1997_2023.RData")
load("data/raw/api_pull_BEA/GrossOutput_ValueAdded.RData")
load("data/cleaned/BEA_ILPAGO_data.RData")
load("data/cleaned/alpha_list.RData")
load("data/cleaned/beta_list.RData")

keep = code_desc_crosswalk[1:66,]
# sort by Code
keep <- keep[order(keep$Code),]
# use dplyr lag
lag <- dplyr::lag


# Omega=expenditure shares ------------------------------------------------

# lapply to each year
Omega_list <- lapply(A_Int_byyear_1997_2023, function(x) {
  exp_shares <- subset(x, Code %in% keep$Code)
  # order rows by Code (IMPORTANT)
  exp_shares <- exp_shares[order(exp_shares$Code),]
  # order columns by Code (IMPORTANT)
  exp_shares <- exp_shares[, c("Code", "Industry Description", keep$Code)]
  exp_matrix <- exp_shares[, 3:(dim(keep)[1]+2)] # drop Code and Industry Description
  Omega <- as.matrix(exp_matrix)
  return(Omega)
})

# L = alpha(beta (I - (diag(1-alpha)Omega)))^-1  --------------------------

L_list = lapply(1:length(alpha_list), function(i) {
  # Y = Y_list[[i]]
  alpha = alpha_list[[i]]$alpha
  Omega = Omega_list[[i]]
  I = diag(1, nrow = dim(keep)[1])
  beta = beta_list[[i]]$beta
  L = alpha * (t(beta) %*% solve(I - (diag(1-alpha) %*% Omega))) 
  return(L)
})

names(L_list) <- names(Omega_list)

# generate TFP changes -------------------------------------

industry_TFP <- tfp_BEA_long %>% select(Code, year, TFP) 
industry_TFP <- merge(keep, industry_TFP, by = "Code")
# generate lags, 1yr and 4yr (use dplyr, order by year)
industry_TFP <- tfp_BEA_long %>% arrange(Code, year) %>%
                                  group_by(Code) %>% 
                                  mutate(delta_tfp_1 = log(TFP) - log(lag(TFP)),
                                         delta_tfp_4 = log(TFP) - log(lag(TFP, 4)))


industry_TFP$year <- as.numeric(as.character(industry_TFP$year))
industry_TFP <- subset(industry_TFP, year > 1997)

# generate elasticity estimates and deltaOmega -----------------------------------------------

load("data/cleaned/main_results.RData")

# extract theta estimates
# Using sector-specific specification!
coef_names <- names(coef(mod_main_sum))
theta_i <- as.data.frame(mod_main_sum$coefficients[grep("delta_logPj_1:", coef_names, value = TRUE)])
colnames(theta_i) <- "beta"
theta_i$coef <- rownames(theta_i)
# # split Code into 2 columns by :
theta_i <- separate(theta_i, coef, into = c("term", "Code"), sep = ":")
# theta_it <- separate(theta_it, Code, into = c("Code", "year"), sep = "-")
theta_i$Code <- gsub("Code", "", theta_i$Code)
theta_i$theta <- 1 - theta_i$beta

# collapse mean and sd of theta by year
# theta_collapsed_Year = theta_it %>%
#   group_by(year) %>%
#   summarise(mean_theta = mean(theta))

# collapse mean and SD of theta by Code
# theta_collapsed_Code = theta_it %>%
#   group_by(Code) %>%
#   summarise(mean_theta = mean(theta),
#             p25_theta = quantile(theta, .25),
#             p75_theta = quantile(theta, .75))

elasticity_byCode <- merge(keep, theta_i, by = "Code")

# set negatives to 1e-6
# elasticity_byyear <- elasticity_byyear %>% select(year, mean_theta)
elasticity_byCode <- elasticity_byCode %>% select(Code, theta)
elasticity_byCode$theta[elasticity_byCode$theta < 0] <- 1e-6
# elasticity_byCode$p25_theta[elasticity_byCode$p25_theta < 0] <- 0.001
# elasticity_byCode$p75_theta[elasticity_byCode$p75_theta < 0] <- 0.001

delta_logOmega <- resid_results %>% 
  mutate(delta_1_resid = resid) %>% 
  select(Code, j, year, delta_1_resid) 

delta_logOmega_list <- split(delta_logOmega, delta_logOmega$year) # get list by year

# Cumulative changes
delta_logOmega_sum <- delta_logOmega %>%
  group_by(Code, j) %>%
  arrange(Code) %>%
  summarise(sum_delta_1_resid = sum(delta_1_resid, na.rm = TRUE))

delta_logOmega_sum_wide <- data.frame(spread(delta_logOmega_sum, j, sum_delta_1_resid))
Codes <- delta_logOmega_sum_wide$Code
delta_logOmega_sum_wide <- delta_logOmega_sum_wide[, -1]
rownames(delta_logOmega_sum_wide) <- Codes
colnames(delta_logOmega_sum_wide) <- Codes
delta_logOmega_sum_wide[is.na(delta_logOmega_sum_wide)] <- 0
cumulative_delta_logOmega_wide <- delta_logOmega_sum_wide
# set diagonal to 0; only interested in changes industry-to-industry, not within industry
diag(cumulative_delta_logOmega_wide) <- 0

# All changes
delta_logOmega_wide_list <- lapply(delta_logOmega_list, function(x) {
  mat <- spread(x, j, delta_1_resid)
  Codes <- mat$Code
  mat <- mat[, -c(1,2)]
  rownames(mat) <- Codes
  colnames(mat) <- Codes
  mat[is.na(mat)] <- 0
  return(mat)
})

# get mean delta logOmega --------------------------------------------

library(fixest)

# subset to last ten years
resid_results_last10 = resid_results %>% filter(year > 2013) %>% mutate(delta_1_resid = resid)

# get mean deltalogOmega by Code-j
delta_logOmega_last10 <- resid_results_last10 %>%
  group_by(Code, j) %>%
  summarise(sum_delta_1_resid = sum(delta_1_resid, na.rm = TRUE))

# reshape to wide, save as predicted (continuing trends of last 10 years)
delta_logOmega_predicted10 <- spread(delta_logOmega_last10, j, sum_delta_1_resid)
Codes <- delta_logOmega_predicted10$Code
delta_logOmega_predicted10 <- delta_logOmega_predicted10[, -1]
rownames(delta_logOmega_predicted10) <- Codes
colnames(delta_logOmega_predicted10) <- Codes
delta_logOmega_predicted10[is.na(delta_logOmega_predicted10)] <- 0
delta_logOmega_predicted10 <- as.data.frame(delta_logOmega_predicted10)
diag(delta_logOmega_predicted10) <- 0 # set diagonal to 0; only interested in changes industry-to-industry, not within industry

# store gross output  -----------------------------------------------------

Y_long = merge(GO_long, keep, by = "Code")
Y_long = subset(Y_long, year <= 2023) # keep only years up to 2023
Y_list = split(Y_long, Y_long$year)
Y_list = lapply(Y_list, function(x) {
  x = as.vector(as.numeric(x$GrossY))
  return(x)
})


# generate HH elasticity --------------------------------------------------

# load data
load("data/cleaned/beta_list.RData")
load("data/cleaned/resid_data_1998_2023.RData") # load api data

# take list of betas and make long
beta_long <- bind_rows(beta_list, .id = "year")
delta_beta <- beta_long %>% 
  mutate(j = Code) %>%
  group_by(Code) %>% arrange(year) %>%
  mutate(delta_1_beta = log(beta) - log(lag(beta, 1))) %>%
  select(Code, j, year, delta_1_beta) %>% filter(year>=1998)

delta_beta <- merge(delta_beta, resid_data_1998_2023, by = c("Code", "j", "year")) %>%
  select(Code, j, year, delta_1_beta, delta_logPj_1)

hh_reg <- feols(delta_1_beta ~ delta_logPj_1 | year, data = delta_beta)
hh_elasticity <- 1 - hh_reg$coefficients["delta_logPj_1"]

# save calibration data ---------------------------------------------------

# save all to a RData
save(Y_list, Omega_list, alpha_list, beta_list, L_list, industry_TFP, elasticity_byCode, cumulative_delta_logOmega_wide, delta_logOmega_wide_list, delta_logOmega_predicted10, hh_elasticity, file = "data/cleaned/structural/calibration_data.RData")




