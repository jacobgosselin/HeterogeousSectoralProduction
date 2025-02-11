# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to data subfolder
setwd("../data")
# load libraries
library(dplyr)
library(tidyr)
library(readxl)
source("../code/helper_fncts.R")

# make sure i use dplyr lag
lag <- dplyr::lag

# load data
load("cleaned/A_Int_byyear_1947_1962.RData")
load("cleaned/A_Int_byyear_1963_1996.RData")
load("cleaned/A_Int_byyear_1997_2022.RData")
load("cleaned/code_crosswalk.RData")

# get output price data ----------------------------------------------------------

# load output price data, 1947-1996
price_BEA <- read_excel("raw/AllTablesHist/GDPbyInd_GO_1947-1997.xlsx", 
                        sheet = "ChainPriceIndexes", skip = 5)
price_BEA$`Industry Description` <- price_BEA$...2
price_BEA <- price_BEA[, c("Industry Description", 1947:1997)]
price_BEA <- merge(code_crosswalk, price_BEA, by = "Industry Description")
price_BEA <- price_BEA[, c("Code", 1947:1997)]
price_BEA <- price_BEA %>%
  mutate(across(-Code, as.numeric))

price_BEA_long <- reshape2::melt(price_BEA, id.vars = c("Code"), variable.name = "year", value.name = "price")
price_BEA_long$price <- as.numeric(price_BEA_long$price)
price_BEA_long_1947_1996 <- price_BEA_long

# load output price data, 1997-2022
price_BEA <- read_excel("../data/raw/GrossOutput.xlsx", 
                        sheet = "TGO104-A", skip = 7)
price_BEA$`Industry Description` <- price_BEA$...2 
price_BEA <- price_BEA[, c("Industry Description", 1997:2023)]
price_BEA <- merge(code_crosswalk, price_BEA, by = "Industry Description")
price_BEA <- price_BEA[, c("Code", 1997:2023)]
price_BEA$'2023' <- as.numeric(price_BEA$'2023') # for some reason reads 2023 as char initially
# # divide every column but Code by the 1997 values (reset base year to 1997)
# price_BEA <- price_BEA %>%
#   mutate(across(-Code, ~ . / price_BEA$`1997`)) 

price_BEA_long <- reshape2::melt(price_BEA, id.vars = c("Code"), variable.name = "year", value.name = "price")
price_BEA_long$price <- as.numeric(price_BEA_long$price)
price_BEA_long_1997_2022 <- price_BEA_long

# get intermediate bundle price data ---------------------------------------------

# load price of II bundle, 1947-1996
int_price_BEA <- read_excel("raw/AllTablesHist/GDPbyInd_II_1947-1997.xlsx", 
                            sheet = "ChainPriceIndexes", skip = 5)
int_price_BEA$`Industry Description` <- int_price_BEA$...2
int_price_BEA <- int_price_BEA[, c("Industry Description", 1947:1997)]
int_price_BEA <- merge(code_crosswalk, int_price_BEA, by = "Industry Description")
int_price_BEA <- int_price_BEA[, c("Code", 1947:1997)]
int_price_BEA <- int_price_BEA %>%
  mutate(across(-Code, as.numeric))
int_price_BEA_long <- reshape2::melt(int_price_BEA, id.vars = c("Code"), variable.name = "year", value.name = "int_price")
int_price_BEA_long_1947_1996 <- int_price_BEA_long

# load price of II bundle, 1997-2022
int_price_BEA <- read_excel("../data/raw/IntermediateInputs.xlsx", 
                            sheet = "TII104-A", skip = 6)
int_price_BEA$`Industry Description` <- int_price_BEA$...2
int_price_BEA <- int_price_BEA[, c("Industry Description", 1997:2023)]
int_price_BEA <- merge(code_crosswalk, int_price_BEA, by = "Industry Description")
int_price_BEA <- int_price_BEA[, c("Code", 1997:2023)]
int_price_BEA$'2023' <- as.numeric(int_price_BEA$'2023') # for some reason reads 2023 as char initially

# # divide every column but Code by the 1997 values (reset base year to 1997)
# int_price_BEA <- int_price_BEA %>%
#   mutate(across(-Code, ~ . / int_price_BEA$`1997`))

int_price_BEA_long <- reshape2::melt(int_price_BEA, id.vars = c("Code"), variable.name = "year", value.name = "int_price")
int_price_BEA_long_1997_2022 <- int_price_BEA_long


# merge output and II bundle prices, and expand ---------------------------

temp_1947_1996 <- merge(price_BEA_long_1947_1996, int_price_BEA_long_1947_1996, by = c("Code", "year"))
temp_1997_2022 <- merge(price_BEA_long_1997_2022, int_price_BEA_long_1997_2022, by = c("Code", "year"))

# fully join temp long datasets with price_BEA_long
# resulting dataset has output price and II bundle price for i, output price for j, across ijt years
expanded_prices_1947_1996 <- temp_1947_1996 %>%
  # Create a unique identifier for each row before the join to preserve original rows
  mutate(id = row_number()) %>%
  # Cross join with price_BEA_long based on year to get every combination of Code and j (other Code) in the same year
  full_join(price_BEA_long_1947_1996, by = "year", suffix = c("", "_j"), relationship = "many-to-many") %>%
  # Remove the temporary id column
  select(-id)

expanded_prices_1947_1996$j <- expanded_prices_1947_1996$Code_j

expanded_prices_1997_2022 <- temp_1997_2022 %>%
  # Create a unique identifier for each row before the join to preserve original rows
  mutate(id = row_number()) %>%
  # Cross join with price_BEA_long based on year to get every combination of Code and j (other Code) in the same year
  full_join(price_BEA_long_1997_2022, by = "year", suffix = c("", "_j"), relationship = "many-to-many") %>%
  # Remove the temporary id column
  select(-id)

expanded_prices_1997_2022$j <- expanded_prices_1997_2022$Code_j

# generate price ratios, and log price ratios
expanded_prices_1947_1996$level_Pratio_II = expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$int_price # get p_j/p_i_int
expanded_prices_1997_2022$level_Pratio_II = expanded_prices_1997_2022$price_j / expanded_prices_1997_2022$int_price # get p_j/p_i_int
expanded_prices_1947_1996$level_Pratio_Out = expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$price # get p_j/p_i
expanded_prices_1997_2022$level_Pratio_Out = expanded_prices_1997_2022$price_j / expanded_prices_1997_2022$price # get p_j/p_i
expanded_prices_1947_1996$log_Pratio_II = log(expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$int_price) # get p_j/p_i_int
expanded_prices_1997_2022$log_Pratio_II = log(expanded_prices_1997_2022$price_j / expanded_prices_1997_2022$int_price) # get p_j/p_i_int
expanded_prices_1947_1996$log_Pratio_Out = log(expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$price) # get p_j/p_i
expanded_prices_1997_2022$log_Pratio_Out = log(expanded_prices_1997_2022$price_j / expanded_prices_1997_2022$price) # get p_j/p_i

# generate lags and change in ratios (levels and logs) at 1 yr
expanded_prices_1947_1996 <- expanded_prices_1947_1996 %>%
  group_by(Code, j) %>%
  arrange(year) %>%
  mutate(
    level_Pratio_1_II = lag(level_Pratio_II, 1),
    level_Pratio_1_Out = lag(level_Pratio_Out, 1),
    delta_levelPratio_1_II = level_Pratio_II - lag(level_Pratio_II, 1),
    delta_levelPratio_1_Out = level_Pratio_Out - lag(level_Pratio_Out, 1),
    logPratio_1_II = lag(log_Pratio_II, 1),
    logPratio_1_Out = lag(log_Pratio_Out, 1),
    delta_logPratio_1_II = log_Pratio_II - lag(log_Pratio_II, 1),
    delta_logPratio_1_Out = log_Pratio_Out - lag(log_Pratio_Out, 1)
  )

expanded_prices_1997_2022 <- expanded_prices_1997_2022 %>%
  group_by(Code, j) %>%
  arrange(year) %>%
  mutate(
    level_Pratio_1_II = lag(level_Pratio_II, 1),
    level_Pratio_1_Out = lag(level_Pratio_Out, 1),
    delta_levelPratio_1_II = level_Pratio_II - lag(level_Pratio_II, 1),
    delta_levelPratio_1_Out = level_Pratio_Out - lag(level_Pratio_Out, 1),
    logPratio_1_II = lag(log_Pratio_II, 1),
    logPratio_1_Out = lag(log_Pratio_Out, 1),
    delta_logPratio_1_II = log_Pratio_II - lag(log_Pratio_II, 1),
    delta_logPratio_1_Out = log_Pratio_Out - lag(log_Pratio_Out, 1)
  )

save(expanded_prices_1947_1996, file = "cleaned/expanded_prices_1947_1996.RData")
save(expanded_prices_1997_2022, file = "cleaned/expanded_prices_1997_2022.RData")

# merge with int expenditure share to make resid dataset ------------------

# ijt data, 1947-1962
Adelta1_Int_byyear_1948_1962 <- lapply(1948:1962, gen_delta, list = A_Int_byyear_1947_1962, horizon = 1, log_change = TRUE)
names(Adelta1_Int_byyear_1948_1962) <- as.character(1948:1962)
Adelta1_Int_long_1948_1962 <- make_mat_list_long(Adelta1_Int_byyear_1948_1962)
Adelta1_Int_long_1948_1962$horizon <- "1yr"
Adelta1_Int_long_1948_1962$delta_1 <- Adelta1_Int_long_1948_1962$val

Adelta5_Int_byyear_1952_1962 <- lapply(1952:1962, gen_delta, list = A_Int_byyear_1947_1962, horizon = 5, log_change = TRUE)
names(Adelta5_Int_byyear_1952_1962) <- as.character(1952:1962)
Adelta5_Int_long_1952_1962 <- make_mat_list_long(Adelta5_Int_byyear_1952_1962)
Adelta5_Int_long_1952_1962$horizon <- "5yrs"
Adelta5_Int_long_1952_1962$delta_5 <- Adelta5_Int_long_1952_1962$val

Adelta10_Int_byyear_1957_1962 <- lapply(1957:1962, gen_delta, list = A_Int_byyear_1947_1962, horizon = 10, log_change = TRUE)
names(Adelta10_Int_byyear_1957_1962) <- as.character(1957:1962)
Adelta10_Int_long_1957_1962 <- make_mat_list_long(Adelta10_Int_byyear_1957_1962)
Adelta10_Int_long_1957_1962$horizon <- "10yrs"
Adelta10_Int_long_1957_1962$delta_10 <- Adelta10_Int_long_1957_1962$val

# ijt data, 1963-1996
Adelta1_Int_byyear_1964_1996 <- lapply(1964:1996, gen_delta, list = A_Int_byyear_1963_1996, horizon = 1, log_change = TRUE)
names(Adelta1_Int_byyear_1964_1996) <- as.character(1964:1996)
Adelta1_Int_long_1964_1996 <- make_mat_list_long(Adelta1_Int_byyear_1964_1996)
Adelta1_Int_long_1964_1996$horizon <- "1yr"
Adelta1_Int_long_1964_1996$delta_1 <- Adelta1_Int_long_1964_1996$val

Adelta5_Int_byyear_1968_1996 <- lapply(1968:1996, gen_delta, list = A_Int_byyear_1963_1996, horizon = 5, log_change = TRUE)
names(Adelta5_Int_byyear_1968_1996) <- as.character(1968:1996)
Adelta5_Int_long_1968_1996 <- make_mat_list_long(Adelta5_Int_byyear_1968_1996)
Adelta5_Int_long_1968_1996$horizon <- "5yrs"
Adelta5_Int_long_1968_1996$delta_5 <- Adelta5_Int_long_1968_1996$val

Adelta10_Int_byyear_1973_1996 <- lapply(1973:1996, gen_delta, list = A_Int_byyear_1963_1996, horizon = 10, log_change = TRUE)
names(Adelta10_Int_byyear_1973_1996) <- as.character(1973:1996)
Adelta10_Int_long_1973_1996 <- make_mat_list_long(Adelta10_Int_byyear_1973_1996)
Adelta10_Int_long_1973_1996$horizon <- "10yrs"
Adelta10_Int_long_1973_1996$delta_10 <- Adelta10_Int_long_1973_1996$val

# ijt data, 1998-2022
Adelta1_Int_byyear_1998_2022 <- lapply(1998:2022, gen_delta, list = A_Int_byyear_1997_2022, horizon = 1, log_change = TRUE)
names(Adelta1_Int_byyear_1998_2022) <- as.character(1998:2022)
Adelta1_Int_long_1998_2022 <- make_mat_list_long(Adelta1_Int_byyear_1998_2022)
Adelta1_Int_long_1998_2022$horizon <- "1yr"
Adelta1_Int_long_1998_2022$delta_1 <- Adelta1_Int_long_1998_2022$val

Adelta5_Int_byyear_2002_2022 <- lapply(2002:2022, gen_delta, list = A_Int_byyear_1997_2022, horizon = 5, log_change = TRUE)
names(Adelta5_Int_byyear_2002_2022) <- as.character(2002:2022)
Adelta5_Int_long_2002_2022 <- make_mat_list_long(Adelta5_Int_byyear_2002_2022)
Adelta5_Int_long_2002_2022$horizon <- "5yrs"
Adelta5_Int_long_2002_2022$delta_5 <- Adelta5_Int_long_2002_2022$val

Adelta10_Int_byyear_2007_2022 <- lapply(2007:2022, gen_delta, list = A_Int_byyear_1997_2022, horizon = 10, log_change = TRUE)
names(Adelta10_Int_byyear_2007_2022) <- as.character(2007:2022)
Adelta10_Int_long_2007_2022 <- make_mat_list_long(Adelta10_Int_byyear_2007_2022)
Adelta10_Int_long_2007_2022$horizon <- "10yrs"
Adelta10_Int_long_2007_2022$delta_10 <- Adelta10_Int_long_2007_2022$val

# resid data 
resid_data_1947_1962 <- merge(Adelta1_Int_long_1948_1962, expanded_prices_1947_1996, by = c("Code", "j", "year"))
resid_data_1963_1996 <- merge(Adelta1_Int_long_1964_1996, expanded_prices_1947_1996, by = c("Code", "j", "year"))
resid_data_1997_2022 <- merge(Adelta1_Int_long_1998_2022, expanded_prices_1997_2022, by = c("Code", "j", "year"))

# refactor (to prevent old levels from remaining)
resid_data_1947_1962$Code <- as.factor(as.character(resid_data_1947_1962$Code))
resid_data_1947_1962$j <- as.factor(as.character(resid_data_1947_1962$j))
resid_data_1963_1996$Code <- as.factor(as.character(resid_data_1963_1996$Code))
resid_data_1963_1996$j <- as.factor(as.character(resid_data_1963_1996$j))
resid_data_1997_2022$Code <- as.factor(as.character(resid_data_1997_2022$Code))
resid_data_1997_2022$j <- as.factor(as.character(resid_data_1997_2022$j))

save(resid_data_1947_1962, file = "cleaned/resid_data_1947_1962.RData")
save(resid_data_1963_1996, file = "cleaned/resid_data_1963_1996.RData")
save(resid_data_1997_2022, file = "cleaned/resid_data_1997_2022.RData")


