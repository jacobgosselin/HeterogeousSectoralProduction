library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)

# make BEA_NAICS crosswalk (detailed) -------------------------------------

# read in BEA Industry and Commodity Codes and NAICS Concordance
detail_naics_crosswalk <- read_excel("data/raw/naics_crosswalk/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx", 
                                      skip = 4)
detail_naics_crosswalk <- detail_naics_crosswalk[, c("Detail", "Related 2017 NAICS Codes")]
colnames(detail_naics_crosswalk) <- c("Code", "NAICS")
detail_naics_crosswalk <- subset(detail_naics_crosswalk, !is.na(detail_naics_crosswalk$NAICS))

detail_naics_crosswalk <- detail_naics_crosswalk %>%
  separate_rows(NAICS, sep = ",") %>%
  mutate(NAICS = str_trim(NAICS))

# remove all digits after dashes (they represent BEA subcategories, which I can't handle)
detail_naics_crosswalk$NAICS <- gsub("-.*", "", detail_naics_crosswalk$NAICS)

# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
# if less than 6 digits, leave blank
detail_naics_crosswalk <- detail_naics_crosswalk %>%
  mutate(`NAICS_2` = ifelse(nchar(NAICS) >= 2, substr(NAICS, 1, 2), ""),
         `NAICS_3` = ifelse(nchar(NAICS) >= 3, substr(NAICS, 1, 3), ""),
         `NAICS_4` = ifelse(nchar(NAICS) >= 4, substr(NAICS, 1, 4), ""),
         `NAICS_5` = ifelse(nchar(NAICS) >= 5, substr(NAICS, 1, 5), ""),
         `NAICS_6` = ifelse(nchar(NAICS) >= 6, substr(NAICS, 1, 6), ""))

# read in 2017 NAICS Codes
NAICS_2017 <- read_excel("data/raw/naics_crosswalk/2017_NAICS_Index_File.xlsx")

NAICS_2017 <- NAICS_2017 %>%
  mutate(`NAICS_2` = substr(`NAICS17`, 1, 2),
         `NAICS_3` = substr(`NAICS17`, 1, 3),
         `NAICS_4` = substr(`NAICS17`, 1, 4),
         `NAICS_5` = substr(`NAICS17`, 1, 5),
         `NAICS_6` = substr(`NAICS17`, 1, 6))

# merge the two dataframes
# if not missing 6-digit, merge on 6-digit, else 5-digit, else 4-digit, else 3-digit
# do this by rbinding 4 subsets of Code_naics_crosswalk (inefficient but stumped Copilot with this lmao)
temp_2 = subset(detail_naics_crosswalk, nchar(NAICS) == 2)
temp_3 = subset(detail_naics_crosswalk, nchar(NAICS) == 3)
temp_4 = subset(detail_naics_crosswalk, nchar(NAICS) == 4)
temp_5 = subset(detail_naics_crosswalk, nchar(NAICS) == 5)
temp_6 = subset(detail_naics_crosswalk, nchar(NAICS) == 6)
temp_2 <- merge(temp_2, NAICS_2017, by = "NAICS_2", all.x = TRUE)
temp_2 <- temp_2 %>% select(c('Code', 'NAICS17')) %>% distinct()
temp_3 <- merge(temp_3, NAICS_2017, by = "NAICS_3", all.x = TRUE) 
temp_3 <- temp_3 %>% select(c('Code', 'NAICS17')) %>% distinct()
temp_4 <- merge(temp_4, NAICS_2017, by = "NAICS_4", all.x = TRUE)
temp_4 <- temp_4 %>% select(c('Code', 'NAICS17')) %>% distinct()
temp_5 <- merge(temp_5, NAICS_2017, by = "NAICS_5", all.x = TRUE)
temp_5 <- temp_5 %>% select(c('Code', 'NAICS17')) %>% distinct()
temp_6 <- merge(temp_6, NAICS_2017, by = "NAICS_6", all.x = TRUE)
temp_6 <- temp_6 %>% select(c('Code', 'NAICS17')) %>% distinct()
crosswalk <- rbind(temp_2, temp_3, temp_4, temp_5, temp_6)

temp_23 = subset(crosswalk, substr(Code,1,2) == "23") %>% select(c('Code')) 
crosswalk <- subset(crosswalk, Code != "23")
temp_23$NAICS_2 <- "23"
NAICS_2017$NAICS_2 <- substr(NAICS_2017$`NAICS17`, 1, 2)
temp_23 <- merge(temp_23, NAICS_2017, by = "NAICS_2", all.x = TRUE)
temp_23 <- temp_23 %>% select(c('Code', 'NAICS17')) %>% distinct()
crosswalk <- rbind(crosswalk, temp_23)

# and now we map forward to 2022 NAICS
# since Compustat is supposed to be up to date
# https://wrds-www.wharton.upenn.edu/pages/support/support-articles/compustat/general/naics-compustat/#detail
# "Column NAICS is always using the most recent NAICS classification framework, across all records, as it is backfilled."

NAICS_2022_2017 <- read_excel("data/raw/naics_crosswalk/2022_to_2017_NAICS.xlsx", skip = 2)
NAICS_2022_2017$NAICS17 <- as.character(NAICS_2022_2017$`2017 NAICS Code`)
NAICS_2022_2017$NAICS22 <- as.character(NAICS_2022_2017$`2022 NAICS Code`)
NAICS_2022_2017 <- NAICS_2022_2017 %>% select(c('NAICS17', 'NAICS22'))
crosswalk <- merge(crosswalk, NAICS_2022_2017, by = "NAICS17", all.x = TRUE)
# order Code, NAICS17, NAICS22
crosswalk <- crosswalk %>% select(c('Code', 'NAICS17', 'NAICS22')) %>% distinct()
BEA_NAICS_crosswalk <- crosswalk

# add Code BEA code to Compustat --------------------------------------------

# Compustat data has some 4-digit, 5-digit, and 6-digit NAICS codes
# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
# if less than 6 digits, leave blank

load("data/raw/api_pull_Compustat/crsp_data.RData")

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
temp_2 <- merge(temp_2, BEA_NAICS_crosswalk %>% select(Code, NAICS_2) %>% distinct(), by = "NAICS_2", all.x = TRUE)
temp_2 <- temp_2 %>% select(c('Code', 'permno', 'naics')) %>% distinct()
temp_3 <- merge(temp_3, BEA_NAICS_crosswalk %>% select(Code, NAICS_3) %>% distinct(), by = "NAICS_3", all.x = TRUE) 
temp_3 <- temp_3 %>% select(c('Code', 'permno', 'naics')) %>% distinct()
temp_4 <- merge(temp_4, BEA_NAICS_crosswalk %>% select(Code, NAICS_4) %>% distinct(), by = "NAICS_4", all.x = TRUE)
temp_4 <- temp_4 %>% select(c('Code', 'permno', 'naics')) %>% distinct()
temp_5 <- merge(temp_5, BEA_NAICS_crosswalk %>% select(Code, NAICS_5) %>% distinct(), by = "NAICS_5", all.x = TRUE)
temp_5 <- temp_5 %>% select(c('Code', 'permno', 'naics')) %>% distinct()
temp_6 <- merge(temp_6, BEA_NAICS_crosswalk %>% select(Code, NAICS_6) %>% distinct(), by = "NAICS_6", all.x = TRUE)
temp_6 <- temp_6 %>% select(c('Code', 'permno', 'naics')) %>% distinct()
permno_Code <- rbind(temp_3, temp_4, temp_5, temp_6)
permno_Code <- subset(permno_Code, !is.na(Code))
# count duplicate permno_Code$permno
permno_duplicates <- permno_Code %>% group_by(permno) %>% summarise(n = n())
# subset to permno with only one Code
permno_Code <- permno_Code %>% filter(permno %in% permno_duplicates$permno[permno_duplicates$n == 1])


# load patent data --------------------------------------------------------

KPSS_2023 <- read_csv("data/raw/KPSS_2023.csv")

patent_data <- KPSS_2023 %>%
  left_join(permno_Code, by = c("permno" = "permno"))

patent_data <- subset(patent_data, !is.na(Code))
patent_data$Code <- patent_data$Code
patent_data$filing_date <- as.Date(as.character(patent_data$filing_date), format = "%Y%m%d")
patent_data$issue_date <- as.Date(as.character(patent_data$issue_date), format = "%Y%m%d")
patent_data$year <- as.numeric(format(patent_data$issue_date, "%Y")) # using filing date as year 
patent_data <- patent_data %>% select(c('patent_num', 'permno', 'Code', 'year', 'xi_real', 'xi_nominal', 'cites'))

patent_data$xi_real = patent_data$xi_real/1000 # billions $
patent_data$xi_nominal = patent_data$xi_nominal/1000 # tens of billions $
patent_data$cites = patent_data$cites/1000 # thousands citations

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

save(patent_data_agg, file = "data/cleaned/patent_data_agg_det.RData")