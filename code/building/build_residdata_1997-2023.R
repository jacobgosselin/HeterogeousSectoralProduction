# load libraries
library(dplyr)
library(tidyr)
library(readxl)
source("code/helper_fncts.R")

# make sure i use dplyr lag
lag <- dplyr::lag

# load data
load("data/cleaned/A_byyear_1997_2023.RData")
load("data/cleaned/A_Int_byyear_1997_2023.RData")
load("data/cleaned/expanded_prices_1997_2023.RData")
load("data/cleaned/code_desc_crosswalk.RData")
exclude = code_desc_crosswalk[67:nrow(code_desc_crosswalk), ] 
keep = subset(code_desc_crosswalk, !(Code %in% exclude$Code))

# merge price data with int expenditure share to make resid dataset ------------------

# delta ijt data, revenue share, 1998-2023
Adelta1_byyear_1998_2023 <- lapply(1998:2023, gen_delta, list = A_byyear_1997_2023, horizon = 1, log_change = TRUE)
names(Adelta1_byyear_1998_2023) <- as.character(1998:2023)
Adelta1_long_1998_2023 <- make_mat_list_long(Adelta1_byyear_1998_2023)
Adelta1_long_1998_2023$horizon <- "1yr"
Adelta1_long_1998_2023 <- Adelta1_long_1998_2023 %>% rename(c(delta_1_Out = val, horizon_Out = horizon))

# delta ijt data, II expenditure cost share, 1998-2023
Adelta1_Int_byyear_1998_2023 <- lapply(1998:2023, gen_delta, list = A_Int_byyear_1997_2023, horizon = 1, log_change = TRUE)
names(Adelta1_Int_byyear_1998_2023) <- as.character(1998:2023)
Adelta1_Int_long_1998_2023 <- make_mat_list_long(Adelta1_Int_byyear_1998_2023)
Adelta1_Int_long_1998_2023$horizon <- "1yr"
# rename val column to delta_1
Adelta1_Int_long_1998_2023 <- Adelta1_Int_long_1998_2023 %>% rename(delta_1 = val)

# ijt data, II expenditure cost share, 1998-2023
A_Int_long_1997_2023 <- make_mat_list_long(A_Int_byyear_1997_2023)

# resid data 
resid_data_1997_2023 <- merge(Adelta1_Int_long_1998_2023, expanded_prices_1997_2023, by = c("Code", "j", "year", "Industry Description"))
resid_data_1997_2023 <- merge(resid_data_1997_2023, Adelta1_long_1998_2023, by = c("Code", "j", "year", "Industry Description"))

# refactor (to prevent old levels from remaining)
resid_data_1997_2023$Code <- as.factor(as.character(resid_data_1997_2023$Code))
resid_data_1997_2023$j <- as.factor(as.character(resid_data_1997_2023$j))

# subset 1999-2023, only data with variance in lagged price ratios (chain measure)
resid_data_1998_2023 <- subset(resid_data_1997_2023, year >= 1998)
resid_data_1998_2023 <- merge(resid_data_1998_2023, A_Int_long_1997_2023, by = c("Code", "j", "year", "Industry Description"))
save(resid_data_1998_2023, file = "data/cleaned/resid_data_1998_2023.RData")
