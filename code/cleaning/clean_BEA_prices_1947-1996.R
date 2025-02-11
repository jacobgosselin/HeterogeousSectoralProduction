# load libraries
library(dplyr)
library(tidyr)
library(readxl)
source("code/helper_fncts.R")

# make sure i use dplyr lag (this has screwed me before)
lag <- dplyr::lag

# load data
load("data/cleaned/code_desc_crosswalk.RData")
exclude = code_desc_crosswalk[67:nrow(code_desc_crosswalk), ] 
keep = subset(code_desc_crosswalk, !(Code %in% exclude$Code))

# generate 1947-1996 expanded price data (data from BEA prepared spreadsheets) -------------------

# load output price data, 1947-1996
price_BEA <- read_excel("data/raw/BEA_1947-1996/GDPbyInd_GO_1947-1997.xlsx", 
                        sheet = "ChainPriceIndexes", skip = 5)
price_BEA$`Industry Description` <- price_BEA$...2
price_BEA <- price_BEA[, c("Industry Description", 1947:1997)]
price_BEA <- merge(code_desc_crosswalk, price_BEA, by = "Industry Description")
price_BEA <- price_BEA[, c("Code", 1947:1997)]
price_BEA <- price_BEA %>%
  mutate(across(-Code, as.numeric))

price_BEA_long <- reshape2::melt(price_BEA, id.vars = c("Code"), variable.name = "year", value.name = "price")
price_BEA_long$price <- as.numeric(price_BEA_long$price)
price_BEA_long_1947_1996 <- price_BEA_long

# load price of II bundle, 1947-1996
int_price_BEA <- read_excel("data/raw/BEA_1947-1996/GDPbyInd_II_1947-1997.xlsx", 
                            sheet = "ChainPriceIndexes", skip = 5)
int_price_BEA$`Industry Description` <- int_price_BEA$...2
int_price_BEA <- int_price_BEA[, c("Industry Description", 1947:1997)]
int_price_BEA <- merge(code_desc_crosswalk, int_price_BEA, by = "Industry Description")
int_price_BEA <- int_price_BEA[, c("Code", 1947:1997)]
int_price_BEA <- int_price_BEA %>%
  mutate(across(-Code, as.numeric))
int_price_BEA_long <- reshape2::melt(int_price_BEA, id.vars = c("Code"), variable.name = "year", value.name = "int_price")
int_price_BEA_long_1947_1996 <- int_price_BEA_long

# merge output and II bundle prices, and expand

temp_1947_1996 <- merge(price_BEA_long_1947_1996, int_price_BEA_long_1947_1996, by = c("Code", "year"))

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
# generate price ratios, and log price ratios
expanded_prices_1947_1996$level_Pratio_II = expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$int_price # get p_j/p_i_int
expanded_prices_1947_1996$level_Pratio_Out = expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$price # get p_j/p_i
expanded_prices_1947_1996$log_Pratio_II = log(expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$int_price) # get p_j/p_i_int
expanded_prices_1947_1996$log_Pratio_Out = log(expanded_prices_1947_1996$price_j / expanded_prices_1947_1996$price) # get p_j/p_i

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
    delta_logPratio_1_Out = log_Pratio_Out - lag(log_Pratio_Out, 1),
    delta_logPj_1 = log(price_j) - lag(log(price_j), 1)
  )

# save dataset
save(expanded_prices_1947_1996, file = "data/cleaned/expanded_prices_1947_1996.RData")