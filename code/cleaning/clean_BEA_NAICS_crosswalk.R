library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(zoo)

# crosswalk for 1947-1996 -------------------------------------------------

# read in 2017 NAICS Codes
NAICS_2012 <- read_excel("data/raw/naics_crosswalk/2012_NAICS_Index_File.xls")

# read in BEA Industry and Commodity Codes and NAICS Concordance
summary_naics_crosswalk <- read_excel("data/raw/IOTables_1947-1996/IOMake_Before_Redefinitions_1947-1962_Summary.xlsx", sheet = "NAICS Codes", 
                                      skip = 4)
summary_naics_crosswalk <- summary_naics_crosswalk[, c("Sector", "Summary", "Related 2012 NAICS Codes")]
colnames(summary_naics_crosswalk) <- c("Sector", "Summary", "NAICS")
# keep if missing Sector
summary_naics_crosswalk <- subset(summary_naics_crosswalk, is.na(summary_naics_crosswalk$Sector))
# drop fully missing rows (all columns missing)
summary_naics_crosswalk <- summary_naics_crosswalk[!apply(is.na(summary_naics_crosswalk), 1, all),]
# fill Summary column below with last non-missing value
summary_naics_crosswalk$Summary <- na.locf(summary_naics_crosswalk$Summary)
# keep if not missing NAICS
summary_naics_crosswalk <- subset(summary_naics_crosswalk, !is.na(summary_naics_crosswalk$NAICS))
# commas in NAICS signify new rows
summary_naics_crosswalk <- summary_naics_crosswalk %>%
  separate_rows(NAICS, sep = ",") %>%
  mutate(NAICS = str_trim(NAICS)) %>%
  select(-Sector) # don't need Sector anymore

# remove all digits after dashes (they represent BEA subcategories, which I can't handle)
summary_naics_crosswalk$NAICS <- gsub("-.*", "", summary_naics_crosswalk$NAICS)

# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
# if less than 6 digits, leave blank
summary_naics_crosswalk <- summary_naics_crosswalk %>%
  mutate(`NAICS_2` = ifelse(nchar(NAICS) >= 2, substr(NAICS, 1, 2), ""),
         `NAICS_3` = ifelse(nchar(NAICS) >= 3, substr(NAICS, 1, 3), ""),
         `NAICS_4` = ifelse(nchar(NAICS) >= 4, substr(NAICS, 1, 4), ""),
         `NAICS_5` = ifelse(nchar(NAICS) >= 5, substr(NAICS, 1, 5), ""),
         `NAICS_6` = ifelse(nchar(NAICS) >= 6, substr(NAICS, 1, 6), ""))

# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
NAICS_2012 <- NAICS_2012 %>%
  mutate(`NAICS_2` = substr(`NAICS12`, 1, 2),
         `NAICS_3` = substr(`NAICS12`, 1, 3),
         `NAICS_4` = substr(`NAICS12`, 1, 4),
         `NAICS_5` = substr(`NAICS12`, 1, 5),
         `NAICS_6` = substr(`NAICS12`, 1, 6))

# merge the two dataframes
# if not missing 6-digit, merge on 6-digit, else 5-digit, else 4-digit, else 3-digit
# do this by rbinding 4 subsets of summary_naics_crosswalk (inefficient but stumped Copilot with this lmao)
temp_2 = subset(summary_naics_crosswalk, nchar(NAICS) == 2)
temp_3 = subset(summary_naics_crosswalk, nchar(NAICS) == 3)
temp_4 = subset(summary_naics_crosswalk, nchar(NAICS) == 4)
temp_5 = subset(summary_naics_crosswalk, nchar(NAICS) == 5)
temp_6 = subset(summary_naics_crosswalk, nchar(NAICS) == 6)
temp_2 <- merge(temp_2, NAICS_2012, by = "NAICS_2", all.x = TRUE)
temp_2 <- temp_2 %>% select(c('Summary', 'NAICS12')) %>% distinct()
temp_3 <- merge(temp_3, NAICS_2012, by = "NAICS_3", all.x = TRUE) 
temp_3 <- temp_3 %>% select(c('Summary', 'NAICS12')) %>% distinct()
temp_4 <- merge(temp_4, NAICS_2012, by = "NAICS_4", all.x = TRUE)
temp_4 <- temp_4 %>% select(c('Summary', 'NAICS12')) %>% distinct()
temp_5 <- merge(temp_5, NAICS_2012, by = "NAICS_5", all.x = TRUE)
temp_5 <- temp_5 %>% select(c('Summary', 'NAICS12')) %>% distinct()
temp_6 <- merge(temp_6, NAICS_2012, by = "NAICS_6", all.x = TRUE)
temp_6 <- temp_6 %>% select(c('Summary', 'NAICS12')) %>% distinct()
crosswalk <- rbind(temp_2, temp_3, temp_4, temp_5, temp_6)

# last part; merge 23 with NAICS17
# BEA and NAICS define construction meaningfully differently (NAICS~production process, BEA~type of structure)
# But it's fine for summary code that encompasses all of construction
temp_23 = subset(crosswalk, Summary == "23") %>% select(c('Summary')) 
crosswalk <- subset(crosswalk, Summary != "23")
temp_23$NAICS_2 <- "23"
NAICS_2012$NAICS_2 <- substr(NAICS_2012$`NAICS12`, 1, 2)
temp_23 <- merge(temp_23, NAICS_2012, by = "NAICS_2", all.x = TRUE)
temp_23 <- temp_23 %>% select(c('Summary', 'NAICS12')) %>% distinct()
crosswalk <- rbind(crosswalk, temp_23)

# and now we map forward to 2022 NAICS
# since Compustat is supposed to be up to date
# https://wrds-www.wharton.upenn.edu/pages/support/support-articles/compustat/general/naics-compustat/#detail
# "Column NAICS is always using the most recent NAICS classification framework, across all records, as it is backfilled."
NAICS_2017_2012 <- read_excel("data/raw/naics_crosswalk/2017_to_2012_NAICS-Changes.xlsx", skip = 2)
NAICS_2017_2012$NAICS12 <- as.character(NAICS_2017_2012$`2012 NAICS Code`)
NAICS_2017_2012$NAICS17 <- as.character(NAICS_2017_2012$`2017 NAICS Code`)
NAICS_2017_2012 <- NAICS_2017_2012 %>% select(c('NAICS12', 'NAICS17'))
crosswalk <- merge(crosswalk, NAICS_2017_2012, by = "NAICS12", all.x = TRUE)

NAICS_2022_2017 <- read_excel("data/raw/naics_crosswalk/2022_to_2017_NAICS.xlsx", skip = 2)
NAICS_2022_2017$NAICS17 <- as.character(NAICS_2022_2017$`2017 NAICS Code`)
NAICS_2022_2017$NAICS22 <- as.character(NAICS_2022_2017$`2022 NAICS Code`)
NAICS_2022_2017 <- NAICS_2022_2017 %>% select(c('NAICS17', 'NAICS22'))
crosswalk <- merge(crosswalk, NAICS_2022_2017, by = "NAICS17", all.x = TRUE)
# order Summary, NAICS17, NAICS22
crosswalk <- crosswalk %>% select(c('Summary', 'NAICS12', 'NAICS17', 'NAICS22')) %>% distinct()
crosswalk <- crosswalk %>% rename(Summary_pre1997 = Summary)
BEA_NAICS_crosswalk_pre1997 <- crosswalk

# crosswalk for 1997-2023 -------------------------------------------------

# read in 2017 NAICS Codes
NAICS_2017 <- read_excel("data/raw/naics_crosswalk/2017_NAICS_Index_File.xlsx")

# read in BEA Industry and Commodity Codes and NAICS Concordance
summary_naics_crosswalk <- read_excel("data/raw/naics_crosswalk/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx", 
                                      skip = 4)
summary_naics_crosswalk <- summary_naics_crosswalk[, c("Sector", "Summary", "Related 2017 NAICS Codes")]
colnames(summary_naics_crosswalk) <- c("Sector", "Summary", "NAICS")
# keep if missing Sector
summary_naics_crosswalk <- subset(summary_naics_crosswalk, is.na(summary_naics_crosswalk$Sector))
# drop fully missing rows (all columns missing)
summary_naics_crosswalk <- summary_naics_crosswalk[!apply(is.na(summary_naics_crosswalk), 1, all),]
# fill Summary column below with last non-missing value
summary_naics_crosswalk$Summary <- na.locf(summary_naics_crosswalk$Summary)
# keep if not missing NAICS
summary_naics_crosswalk <- subset(summary_naics_crosswalk, !is.na(summary_naics_crosswalk$NAICS))
# commas in NAICS signify new rows
summary_naics_crosswalk <- summary_naics_crosswalk %>%
  separate_rows(NAICS, sep = ",") %>%
  mutate(NAICS = str_trim(NAICS)) %>%
  select(-Sector) # don't need Sector anymore

# remove all digits after dashes (they represent BEA subcategories, which I can't handle)
summary_naics_crosswalk$NAICS <- gsub("-.*", "", summary_naics_crosswalk$NAICS)

# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
# if less than 6 digits, leave blank
summary_naics_crosswalk <- summary_naics_crosswalk %>%
  mutate(`NAICS_2` = ifelse(nchar(NAICS) >= 2, substr(NAICS, 1, 2), ""),
         `NAICS_3` = ifelse(nchar(NAICS) >= 3, substr(NAICS, 1, 3), ""),
         `NAICS_4` = ifelse(nchar(NAICS) >= 4, substr(NAICS, 1, 4), ""),
         `NAICS_5` = ifelse(nchar(NAICS) >= 5, substr(NAICS, 1, 5), ""),
         `NAICS_6` = ifelse(nchar(NAICS) >= 6, substr(NAICS, 1, 6), ""))

# save as provided BEA_NAICS_crosswalk
# (no longer needed)
BEA_NAICS_crosswalk_provided <- summary_naics_crosswalk
save(BEA_NAICS_crosswalk_provided, file = "data/cleaned/BEA_NAICS_crosswalk_provided.RData")

# generate NAICS 3-digit, 4-digit, 5-digit, and 6-digit by truncating NAICS
NAICS_2017 <- NAICS_2017 %>%
  mutate(`NAICS_2` = substr(`NAICS17`, 1, 2),
         `NAICS_3` = substr(`NAICS17`, 1, 3),
         `NAICS_4` = substr(`NAICS17`, 1, 4),
         `NAICS_5` = substr(`NAICS17`, 1, 5),
         `NAICS_6` = substr(`NAICS17`, 1, 6))

# merge the two dataframes
# if not missing 6-digit, merge on 6-digit, else 5-digit, else 4-digit, else 3-digit
# do this by rbinding 4 subsets of summary_naics_crosswalk (inefficient but stumped Copilot with this lmao)
temp_2 = subset(summary_naics_crosswalk, nchar(NAICS) == 2)
temp_3 = subset(summary_naics_crosswalk, nchar(NAICS) == 3)
temp_4 = subset(summary_naics_crosswalk, nchar(NAICS) == 4)
temp_5 = subset(summary_naics_crosswalk, nchar(NAICS) == 5)
temp_6 = subset(summary_naics_crosswalk, nchar(NAICS) == 6)
temp_2 <- merge(temp_2, NAICS_2017, by = "NAICS_2", all.x = TRUE)
temp_2 <- temp_2 %>% select(c('Summary', 'NAICS17')) %>% distinct()
temp_3 <- merge(temp_3, NAICS_2017, by = "NAICS_3", all.x = TRUE) 
temp_3 <- temp_3 %>% select(c('Summary', 'NAICS17')) %>% distinct()
temp_4 <- merge(temp_4, NAICS_2017, by = "NAICS_4", all.x = TRUE)
temp_4 <- temp_4 %>% select(c('Summary', 'NAICS17')) %>% distinct()
temp_5 <- merge(temp_5, NAICS_2017, by = "NAICS_5", all.x = TRUE)
temp_5 <- temp_5 %>% select(c('Summary', 'NAICS17')) %>% distinct()
temp_6 <- merge(temp_6, NAICS_2017, by = "NAICS_6", all.x = TRUE)
temp_6 <- temp_6 %>% select(c('Summary', 'NAICS17')) %>% distinct()
crosswalk <- rbind(temp_2, temp_3, temp_4, temp_5, temp_6)

# last part; merge 23 with NAICS17
# BEA and NAICS define construction meaningfully differently (NAICS~production process, BEA~type of structure)
# But it's fine for summary code that encompasses all of construction
temp_23 = subset(crosswalk, Summary == "23") %>% select(c('Summary')) 
crosswalk <- subset(crosswalk, Summary != "23")
temp_23$NAICS_2 <- "23"
NAICS_2017$NAICS_2 <- substr(NAICS_2017$`NAICS17`, 1, 2)
temp_23 <- merge(temp_23, NAICS_2017, by = "NAICS_2", all.x = TRUE)
temp_23 <- temp_23 %>% select(c('Summary', 'NAICS17')) %>% distinct()
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
# order Summary, NAICS17, NAICS22
crosswalk <- crosswalk %>% select(c('Summary', 'NAICS17', 'NAICS22')) %>% distinct()
BEA_NAICS_crosswalk <- crosswalk


# merge pre 1997 and post 1997, keep perfect matches ----------------------

crosswalk <- merge(BEA_NAICS_crosswalk_pre1997, BEA_NAICS_crosswalk, by = "NAICS22")
crosswalk <- subset(crosswalk, Summary_pre1997 == Summary)
crosswalk <- crosswalk %>% select(c('NAICS22', 'Summary'))
# keep only unique rows (duplicates generated in this step)
crosswalk <- crosswalk %>% distinct()
BEA_NAICS_crosswalk <- crosswalk

# save
save(BEA_NAICS_crosswalk, file = "data/cleaned/BEA_NAICS_crosswalk_constructed.RData")