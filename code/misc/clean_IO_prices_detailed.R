# load libraries
library(dplyr)
library(tidyr)
library(readxl)
source("code/helper_fncts.R")
source("code/detailed/helper_fncts_det.R")

# create A_byyear_det list
years <- as.character(c(2007, 2012, 2017))
A_byyear_det <- lapply(years, wrapper)
names(A_byyear_det) <- years

# delta ijt data, revenue share, 2007-2017
A_byyear_det_long <- make_mat_list_long(A_byyear_det)
A_delta5_long_det <- A_byyear_det_long %>%
  group_by(Code, j) %>%
  arrange(Code, j, year) %>%
  mutate(delta_5 = log(val) - log(lag(val))) 
top_25 <- A_delta5_long_det %>%
  group_by(Code, year) %>%
  top_n(25, val)

# read in data
price_indices <- read_excel("data/raw/BEA_Detailed/GrossOutput.xlsx", sheet = "UGO304-A", skip = 7)
colnames(price_indices)[2] <- "Industry Description"
price_indices <- price_indices[,c("Industry Description", 1997:2023)]

# reshape long, i = "Industry Description", year 
price_indices_long <- price_indices %>%
  pivot_longer(cols = -"Industry Description", names_to = "year", values_to = "price_index") %>%
  mutate(year = as.numeric(year))

# generate 5-year log-change in prices
# order by year
price_indices_long <- price_indices_long %>%
  arrange(`Industry Description`, year) %>%
  group_by(`Industry Description`) %>%
  mutate(price_index_lag = lag(price_index, 5),
         delta_logPj_5 = log(price_index) - log(price_index_lag)) %>%
  select(`Industry Description`, year, delta_logPj_5)

# select unique Code-Industry Description  from Adelta5_long_det
unique_industry <- Adelta5_long_det %>%
  select(`Code`, `Industry Description`) %>%
  distinct()

# merge for price_indices_long, keep Code as j
price_indices_long <- merge(unique_industry, price_indices_long, by = "Industry Description")
price_indices_long$j <- price_indices_long$Code
price_indices_long <- price_indices_long[, c("j", "year", "delta_logPj_5")]

# merge for resid_data
resid_data_det <- merge(top_25, price_indices_long, by = c("j", "year"))

# save
save(resid_data_det, file = "data/cleaned/resid_data_det.RData")