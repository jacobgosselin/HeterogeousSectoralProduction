# load libraries and helper fncts
library(tidyr)
library(dplyr)
source("code/helper_fncts.R")
load("data/cleaned/pct_defense_byyear_1997_2023.RData")
load("data/cleaned/A_Cust_byyear_1997_2023.RData")
load("data/cleaned/A_int_byyear_1997_2023.RData")


# Load Federal Defense Spending from FRED (BEA is original source)
FRED <- read.csv("data/raw/misc/FDEFX.csv")
FRED$year <- as.numeric(substr(FRED$observation_date, 1, 4))
# develop log change in FDEFX
FRED$delta_logFDEFX <- c(NA, diff(log(FRED$FDEFX)))
FRED <- FRED[, c("year", "delta_logFDEFX")]

# Load Defense use data and make long (industry-year)
pct_defense_long_list <- lapply(names(pct_defense_byyear_1997_2023), function(year) {
  df <- data.frame(t(pct_defense_byyear_1997_2023[[year]]))
  df$year <- year  # Add a new column 'year' with the name of the list item
  df$Code <- rownames(df)
  colnames(df) <- c("pct_defense", "year", "Code")
  return(df)
})
pct_defense_long <- do.call(rbind, pct_defense_long_list)

# Load A_Cust_byyear, i.e. a_ij = P_j X_ij/(P_j Y_j), industry i spending as share of industry j revenue
# Generate Leontief inverse (I-A)^-1, year by year
L_out_customers_list <- lapply(names(A_Cust_byyear_1997_2023), function(year) {
  A_out_customers <- A_Cust_byyear_1997_2023[[year]]
  sub_mat <- A_out_customers[, -c(1, 2, 69)]  # remove Code and Industry Description
  sub_mat <- as.matrix(sub_mat)
  I <- diag(nrow(sub_mat))
  leontief <- solve(I - sub_mat)
  leontief <- as.data.frame(leontief)
  colnames(leontief) <- colnames(sub_mat)
  leontief <- cbind(A_out_customers[, c("Code", "Industry Description")], leontief, A_out_customers["total_industry_output"])
  return(leontief)
})
names(L_out_customers_list) <- names(A_Cust_byyear_1997_2023)

# Convert Leontief matrix year-by-year to long dataframe, l_ijt (industry-industry-year)
L_out_customers_long <- make_mat_list_long(L_out_customers_list)
L_out_customers_long$leontief_customer <- L_out_customers_long$val

# Load A_int_byyear, make long a_ijt (industry-industry-year)
A_Int_byyear_1997_2023_long <- make_mat_list_long(A_Int_byyear_1997_2023)
A_Int_byyear_1997_2023_long$int_exp_share <- A_Int_byyear_1997_2023_long$val

# Put it all together and generate instruments
atalay_instruments <- merge(FRED, pct_defense_long, by = "year")
atalay_instruments <- merge(atalay_instruments, L_out_customers_long, by = c("year", "Code"))
atalay_instruments <- merge(atalay_instruments, A_Int_byyear_1997_2023_long, by = c("year", "Code", "j"))

# Generate military_shock_i = sum_j pct_defense * l_ij * delta_logFDEFX
# Generate military_shock_j = sum_i pct_defense * l_ij * delta_logFDEFX (same but for j)
atalay_instruments$temp_1 = atalay_instruments$pct_defense * atalay_instruments$leontief_customer

atalay_instruments <- atalay_instruments %>% # Gen military_spending_shock_i
  group_by(Code, year) %>%
  mutate(military_shock_i = sum(temp_1) * delta_logFDEFX)

atalay_instruments <- atalay_instruments %>% # Gen military_spending_shock_j
  group_by(j, year) %>%
  mutate(military_shock_j = sum(temp_1) * delta_logFDEFX)

# Generate military_shock_suppliers = sum_j military_shock_j * int_exp_share (weighted shock to industry i's suppliers)
atalay_instruments$temp_2 = atalay_instruments$military_shock_j * atalay_instruments$int_exp_share
atalay_instruments <- atalay_instruments %>% # Gen military_spending_shock_j
  group_by(Code, year) %>%
  mutate(military_shock_suppliers = sum(temp_2))

# Subset and save
atalay_instruments <- subset(atalay_instruments, year >= 1998)
atalay_instruments <- atalay_instruments %>% 
  select(Code, year, j, military_shock_i, military_shock_j, military_shock_suppliers)

save(atalay_instruments, file = "data/cleaned/atalay_instruments.RData") 



    
    
