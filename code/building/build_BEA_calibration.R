# libraries
library(dplyr)
library(tidyr)
library(fixest)
library(zoo)

# load data
load("data/cleaned/code_desc_crosswalk.RData")
load("data/cleaned/A_Int_byyear_1997_2023.RData")
load("data/raw/api_pull_BEA/GrossOutput_ValueAdded.RData")
load("data/cleaned/BEA_ILPAGO_data.RData")
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


# alpha = VA/GO -----------------------------------------------------------

alpha = merge(VA_long, GO_long, by = c("Code", "year")) 
alpha = merge(alpha, keep, by = "Code")
alpha$alpha = alpha$VAdd / alpha$GrossY
alpha = alpha %>% select(Code, year, alpha)
alpha_list = split(alpha, alpha$year)
alpha_list = lapply(alpha_list, function(x) {
  x = as.vector(x$alpha)
  return(x)
})

names(alpha_list) <- names(Omega_list)

# beta =  Y(I - diag(alpha))Omega -----------------------------------------

Y_long = merge(GO_long, keep, by = "Code")
Y_list = split(Y_long, Y_long$year)
Y_list = lapply(Y_list, function(x) {
  x = as.vector(as.numeric(x$GrossY))
  return(x)
})

beta_list = lapply(1:length(Y_list), function(i) {
  Y = Y_list[[i]]
  alpha = alpha_list[[i]]
  Omega = Omega_list[[i]]
  I = diag(1, nrow = dim(keep)[1])
  beta = Y %*% (I - diag(alpha)) %*% Omega
  beta = beta / sum(beta) # normalize to sum to 1
  return(beta)
})

names(beta_list) <- names(Omega_list)

# L = alpha(beta (I - (diag(1-alpha)Omega)))^-1  --------------------------

L_list = lapply(1:length(Y_list), function(i) {
  Y = Y_list[[i]]
  alpha = alpha_list[[i]]
  Omega = Omega_list[[i]]
  I = diag(1, nrow = dim(keep)[1])
  beta = beta_list[[i]]
  L = alpha * (beta %*% solve(I - (diag(1-alpha) %*% Omega))) 
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

load("data/cleaned/resid_results_1998_2023.RData")

# extract theta estimates
coef_names <- names(coef(model_delta_1_sum))
theta_it <- as.data.frame(model_delta_1_sum$coefficients[grep("delta_logPj_1:", coef_names, value = TRUE)])
colnames(theta_it) <- "beta"
theta_it$coef <- rownames(theta_it)
# split Code into 2 columns by :
theta_it <- separate(theta_it, coef, into = c("term", "Code"), sep = ":")
theta_it <- separate(theta_it, Code, into = c("Code", "year"), sep = "-")
theta_it$Code <- gsub("industry_year", "", theta_it$Code)
theta_it$theta <- 1 - theta_it$beta

# collapse mean and sd of theta by year
theta_collapsed_Year = theta_it %>%
  group_by(year) %>%
  summarise(mean_theta = mean(theta))

# collapse mean and SD of theta by Code
theta_collapsed_Code = theta_it %>%
  group_by(Code) %>%
  summarise(mean_theta = mean(theta),
            p25_theta = quantile(theta, .25),
            p75_theta = quantile(theta, .75))

elasticity_byyear <- theta_collapsed_Year
elasticity_byCode <- merge(keep, theta_collapsed_Code, by = "Code")

# set negatives to 0.001
elasticity_byyear <- elasticity_byyear %>% select(year, mean_theta)
elasticity_byCode <- elasticity_byCode %>% select(Code, mean_theta, p25_theta, p75_theta)
elasticity_byyear$mean_theta[elasticity_byyear$mean_theta < 0] <- 0.001
elasticity_byCode$mean_theta[elasticity_byCode$mean_theta < 0] <- 0.001
elasticity_byCode$p25_theta[elasticity_byCode$p25_theta < 0] <- 0.001
elasticity_byCode$p75_theta[elasticity_byCode$p75_theta < 0] <- 0.001

delta_logOmega <- resid_results %>% select(Code, j, year, delta_1_resid)
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
resid_results_last10 = resid_results %>% filter(year > 2013)

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



# save calibration data ---------------------------------------------------

# save all to a RData
save(Y_list, Omega_list, alpha_list, beta_list, L_list, industry_TFP, elasticity_byyear, elasticity_byCode, cumulative_delta_logOmega_wide, delta_logOmega_wide_list, delta_logOmega_predicted10, file = "data/cleaned/structural/calibration_data.RData")




