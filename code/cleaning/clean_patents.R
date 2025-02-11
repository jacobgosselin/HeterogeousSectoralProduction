# load libraries
library(readxl)
library(dplyr)
library(stringr)
library(RPostgres)
library(data.table)
library(readr)
library(tidyr)
library(dplyr)
library(zoo)

# load data
load("data/raw/api_pull_Compustat/crsp_data.RData")
load("data/cleaned/BEA_NAICS_crosswalk_constructed.RData")

# add summary BEA code to Compustat --------------------------------------------

# Compustat data has some 4-digit, 5-digit, and 6-digit NAICS codes
# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
# if less than 6 digits, leave blank

crsp_data <- crsp_data %>%
  mutate(`NAICS_2` = ifelse(nchar(naics) == 2, substr(naics, 1, 2), ""),
         `NAICS_3` = ifelse(nchar(naics) == 3, substr(naics, 1, 3), ""),
         `NAICS_4` = ifelse(nchar(naics) == 4, substr(naics, 1, 4), ""),
         `NAICS_5` = ifelse(nchar(naics) == 5, substr(naics, 1, 5), ""),
         `NAICS_6` = ifelse(nchar(naics) == 6, substr(naics, 1, 6), ""))

BEA_NAICS_crosswalk$naics = BEA_NAICS_crosswalk$NAICS22
BEA_NAICS_crosswalk <- BEA_NAICS_crosswalk %>%
  mutate(`NAICS_2` = substr(naics, 1, 2),
         `NAICS_3` = substr(naics, 1, 3),
         `NAICS_4` = substr(naics, 1, 4),
         `NAICS_5` = substr(naics, 1, 5),
         `NAICS_6` = substr(naics, 1, 6))

temp_2 <- subset(crsp_data, nchar(naics)==2)
temp_3 <- subset(crsp_data, nchar(naics)==3)
temp_4 <- subset(crsp_data, nchar(naics)==4)
temp_5 <- subset(crsp_data, nchar(naics)==5)
temp_6 <- subset(crsp_data, nchar(naics)==6)
temp_2 <- merge(temp_2, BEA_NAICS_crosswalk %>% select(Summary, NAICS_2) %>% distinct(), by = "NAICS_2", all.x = TRUE)
temp_2 <- temp_2 %>% select(c('Summary', 'permno', 'naics')) %>% distinct()
temp_3 <- merge(temp_3, BEA_NAICS_crosswalk %>% select(Summary, NAICS_3) %>% distinct(), by = "NAICS_3", all.x = TRUE) 
temp_3 <- temp_3 %>% select(c('Summary', 'permno', 'naics')) %>% distinct()
temp_4 <- merge(temp_4, BEA_NAICS_crosswalk %>% select(Summary, NAICS_4) %>% distinct(), by = "NAICS_4", all.x = TRUE)
temp_4 <- temp_4 %>% select(c('Summary', 'permno', 'naics')) %>% distinct()
temp_5 <- merge(temp_5, BEA_NAICS_crosswalk %>% select(Summary, NAICS_5) %>% distinct(), by = "NAICS_5", all.x = TRUE)
temp_5 <- temp_5 %>% select(c('Summary', 'permno', 'naics')) %>% distinct()
temp_6 <- merge(temp_6, BEA_NAICS_crosswalk %>% select(Summary, NAICS_6) %>% distinct(), by = "NAICS_6", all.x = TRUE)
temp_6 <- temp_6 %>% select(c('Summary', 'permno', 'naics')) %>% distinct()
permno_summary <- rbind(temp_3, temp_4, temp_5, temp_6)
permno_summary <- subset(permno_summary, !is.na(Summary))
# count duplicate permno_summary$permno
permno_duplicates <- permno_summary %>% group_by(permno) %>% summarise(n = n())
# subset to permno with only one Summary
permno_summary <- permno_summary %>% filter(permno %in% permno_duplicates$permno[permno_duplicates$n == 1])


# load patent data --------------------------------------------------------

KPSS_2023 <- read_csv("data/raw/KPSS_2023.csv")

patent_data <- KPSS_2023 %>%
  left_join(permno_summary, by = c("permno" = "permno"))

patent_data <- subset(patent_data, !is.na(Summary))
patent_data$Code <- patent_data$Summary
patent_data$filing_date <- as.Date(as.character(patent_data$filing_date), format = "%Y%m%d")
patent_data$issue_date <- as.Date(as.character(patent_data$issue_date), format = "%Y%m%d")
patent_data$year <- as.numeric(format(patent_data$issue_date, "%Y")) # using filing date as year 
patent_data <- patent_data %>% select(c('patent_num', 'permno', 'Code', 'year', 'xi_real', 'xi_nominal', 'cites'))

patent_data$xi_real = patent_data$xi_real/1000 # billions $
patent_data$xi_nominal = patent_data$xi_nominal/1000 # tens of billions $
patent_data$cites = patent_data$cites/1000 # thousands citations

save(patent_data, file = "data/cleaned/patent_data.RData")


# aggregate patent_data ---------------------------------------------------

# collapse to codeXyear; count number of rows, and sum xi_real, xi_nominal, and cites
patent_data_agg <- patent_data %>%
  group_by(Code, year) %>%
  summarise(
    patents_num = n(),
    patents_xi_real = sum(xi_real),
    patents_xi_nominal = sum(xi_nominal),
    patents_cites = sum(cites)
  ) %>%
  ungroup()

# Create a complete grid of all possible Code-year combinations
years <- seq(min(patent_data_agg$year, na.rm=TRUE), max(patent_data_agg$year, na.rm = TRUE))
codes <- unique(patent_data_agg$Code)
complete_grid <- expand.grid(Code = codes, year = years)

# Merge with the complete grid and replace NAs with 0
patent_data_agg <- complete_grid %>%
  left_join(patent_data_agg, by = c("Code", "year")) %>%
  replace_na(list(
    patents_num = 0,
    patents_xi_real = 0,
    patents_xi_nominal = 0,
    patents_cites = 0
  ))

save(patent_data_agg, file = "data/cleaned/patent_data_agg.RData")


