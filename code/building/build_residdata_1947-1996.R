# load libraries
library(dplyr)
library(tidyr)
library(readxl)
source("code/helper_fncts.R")

# make sure i use dplyr lag
lag <- dplyr::lag

# load data
load("data/cleaned/A_Int_byyear_1947_1962.RData")
load("data/cleaned/A_Int_byyear_1963_1996.RData")
load("data/cleaned/code_desc_crosswalk.RData")
load("data/cleaned/expanded_prices_1947_1996.RData")

# merge with int expenditure share to make resid dataset ------------------

# ijt data, 1947-1962
Adelta1_Int_byyear_1948_1962 <- lapply(1948:1962, gen_delta, list = A_Int_byyear_1947_1962, horizon = 1, log_change = TRUE)
names(Adelta1_Int_byyear_1948_1962) <- as.character(1948:1962)
Adelta1_Int_long_1948_1962 <- make_mat_list_long(Adelta1_Int_byyear_1948_1962)
Adelta1_Int_long_1948_1962$horizon <- "1yr"
Adelta1_Int_long_1948_1962 <- Adelta1_Int_long_1948_1962 %>% rename(delta_1 = val)

# ijt data, 1963-1996
Adelta1_Int_byyear_1964_1996 <- lapply(1964:1996, gen_delta, list = A_Int_byyear_1963_1996, horizon = 1, log_change = TRUE)
names(Adelta1_Int_byyear_1964_1996) <- as.character(1964:1996)
Adelta1_Int_long_1964_1996 <- make_mat_list_long(Adelta1_Int_byyear_1964_1996)
Adelta1_Int_long_1964_1996$horizon <- "1yr"
Adelta1_Int_long_1964_1996 <- Adelta1_Int_long_1964_1996 %>% rename(delta_1 = val)

# resid data 
resid_data_1948_1962 <- merge(Adelta1_Int_long_1948_1962, expanded_prices_1947_1996, by = c("Code", "j", "year"))
resid_data_1964_1996 <- merge(Adelta1_Int_long_1964_1996, expanded_prices_1947_1996, by = c("Code", "j", "year"))

# refactor (to prevent old levels from remaining)
resid_data_1948_1962$Code <- as.factor(as.character(resid_data_1948_1962$Code))
resid_data_1948_1962$j <- as.factor(as.character(resid_data_1948_1962$j))
resid_data_1964_1996$Code <- as.factor(as.character(resid_data_1964_1996$Code))
resid_data_1964_1996$j <- as.factor(as.character(resid_data_1964_1996$j))

save(resid_data_1948_1962, file = "data/cleaned/resid_data_1948_1962.RData")
save(resid_data_1964_1996, file = "data/cleaned/resid_data_1964_1996.RData")


