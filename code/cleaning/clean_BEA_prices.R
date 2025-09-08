# load libraries
library(dplyr)
library(tidyr)
library(readxl)
source("code/helper_fncts.R")

# make sure i use dplyr lag
lag <- dplyr::lag

# load data
load("data/cleaned/code_desc_crosswalk.RData")
exclude = code_desc_crosswalk[67:nrow(code_desc_crosswalk), ] 
keep = subset(code_desc_crosswalk, !(Code %in% exclude$Code))


# load api prices ------------------------------------------------------

load("data/raw/api_pull_BEA/price.RData")

output_price_data <- output_price_data_raw
output_price_data$Code <- output_price_data$Industry
output_price_data <- merge(output_price_data, keep)
output_price_data$year <- as.numeric(output_price_data$Year)
output_price_data$price <- as.numeric(output_price_data$DataValue)
output_price_data <- subset(output_price_data, select = c("year", "Code", "Industry Description", "price"))
output_price_data$price <- as.numeric(output_price_data$price)
output_price_data$year <- as.numeric(output_price_data$year)
# by Code-year, divide by 1997 value (reset 1997=100, instead of 2017=100)
output_price_data <- output_price_data %>%
  group_by(Code) %>%
  mutate(price = price / price[year == 1997])
save(output_price_data, file = "data/cleaned/BEA_price_data.RData")

II_price_data <- II_price_data_raw
II_price_data$Code <- II_price_data$Industry
II_price_data <- merge(II_price_data, keep)
II_price_data$year <- as.numeric(II_price_data$Year)
II_price_data$int_price <- as.numeric(II_price_data$DataValue)
II_price_data <- subset(II_price_data, select = c("year", "Code", "Industry Description", "int_price"))
II_price_data$int_price <- as.numeric(II_price_data$int_price)
II_price_data$year <- as.numeric(II_price_data$year)
# by Code-year, divide by 1997 value (reset 1997=100, instead of 2017=100)
II_price_data <- II_price_data %>%
  group_by(Code) %>%
  mutate(int_price = int_price / int_price[year == 1997])

# merge output and II bundle prices, and expand ---------------------------

temp_1997_2023 <- merge(output_price_data, II_price_data, by = c("Code", "year", "Industry Description")) # API

expanded_prices_1997_2023 <- temp_1997_2023 %>% # API
  # Create a unique identifier for each row before the join to preserve original rows
  mutate(id = row_number()) %>%
  # Cross join with price_BEA_long based on year to get every combination of Code and j (other Code) in the same year
  full_join(output_price_data, by = "year", suffix = c("", "_j"), relationship = "many-to-many") %>%
  # Remove the temporary id column
  select(-id)

expanded_prices_1997_2023$j <- expanded_prices_1997_2023$Code_j

# generate price ratios, and log price ratios
expanded_prices_1997_2023$level_Pratio_II = expanded_prices_1997_2023$price_j / expanded_prices_1997_2023$int_price # get p_j/p_i_int
expanded_prices_1997_2023$level_Pratio_Out = expanded_prices_1997_2023$price_j / expanded_prices_1997_2023$price # get p_j/p_i
expanded_prices_1997_2023$log_Pratio_II = log(expanded_prices_1997_2023$price_j / expanded_prices_1997_2023$int_price) # get p_j/p_i_int
expanded_prices_1997_2023$log_Pratio_Out = log(expanded_prices_1997_2023$price_j / expanded_prices_1997_2023$price) # get p_j/p_i
expanded_prices_1997_2023$level_Pratio_Int_Out = expanded_prices_1997_2023$int_price / expanded_prices_1997_2023$price # get p_i_int/p_i
expanded_prices_1997_2023$log_Pratio_Int_Out = log(expanded_prices_1997_2023$int_price / expanded_prices_1997_2023$price) # get p_i_int/p_i

expanded_prices_1997_2023 <- expanded_prices_1997_2023 %>%
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
    delta_logPratio_1_Out = log_Pratio_Out - lag(log_Pratio_Out, 1),
    delta_logPj_1 = log(price_j) - lag(log(price_j), 1),
    delta_logPratio_1_IIOut = log_Pratio_Int_Out - lag(log_Pratio_Int_Out, 1)
  )

save(expanded_prices_1997_2023, file = "data/cleaned/expanded_prices_1997_2023.RData")