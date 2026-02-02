# libraries
library(reshape2)
library(data.table)
library(dplyr)
library(readr)
library(fixest)
library(readxl)
library(stringr)
library(zoo)
setwd('/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/het_sectoral_prod/V2')
# load data
load("clean_data/analysis_data.RData")
load("clean_data/code_desc_crosswalk.RData")
load("raw_data/BEA_defense_spending.RData")


non_govt_industries <- unique(code_desc_crosswalk$Code) %>%
  # filter out T codes, G codes, and Trade + Trans + Imports + adjustments
  .[!grepl("^T", .) & !grepl("^G", .) & !grepl("^M", .) &  . != "Trade" & . != "Trans" & . != "SUB" &
      . != "Used" & . != "Other"]
code_desc_crosswalk <- code_desc_crosswalk %>%
  filter(Code %in% non_govt_industries) %>%
  rename(i = Code,
         Industry = `Industry Description`) %>%
  arrange(i) 

# Load Federal Defense Spending from FRED (BEA is original source)
FRED <- read.csv("raw_data/FDEFX.csv")
FRED$year <- substr(FRED$observation_date, 1, 4)
# develop log change in FDEFX
FRED$delta_logFDEFX <- c(NA, diff(log(FRED$FDEFX)))
FRED <- FRED[, c("year", "delta_logFDEFX")]

# Load TFP Data
tfp_KLEMS <- read_excel("raw_data/major-industry-total-factor-productivity-klems.xlsx", 
                      sheet = "Annual", skip = 2)


# Clean TFP data ===============================

# read in BEA Industry and Commodity Codes and NAICS Concordance
summary_naics_crosswalk <- read_excel("raw_data/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx", 
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
BEA_NAICS_crosswalk_provided <- summary_naics_crosswalk

# annoyingly Descriptions in KLEMS TFP data from BLS don't perfectly match BEA; slightly more aggregated
# crosswalk with NAICS codes using the joint BEA/ILPA sheet, which has NAICS/Descriptions for official BLS TFP
ILPA <- read_excel("raw_data/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
                   sheet = "TFP", skip = 1)
ILPA$NAICS12 <- ILPA$`NAICS 3 digit`
ILPA <- subset(ILPA, !is.na(ILPA$`Industry Description`))
ILPA <- ILPA %>% select(c('NAICS12', 'Industry Description'))

# expand ILPA; each , is a new NAICS code, and we need to expand the dashes
ILPA <- ILPA %>% separate_rows(NAICS12, sep = ",")

expand_ranges <- function(x) {
  if (grepl("-", x)) {
    parts <- strsplit(x, "-")[[1]]
    start <- as.numeric(parts[1])
    end <- as.numeric(parts[2])
    return(paste(seq(start, end), collapse = ", "))
  } else {
    return(x)
  }
}

ILPA$NAICS12 <- sapply(ILPA$NAICS12, expand_ranges)
ILPA <- ILPA %>% separate_rows(NAICS12, sep = ",")

# merge time
ILPA <- ILPA %>%
  mutate(NAICS12 = str_trim(NAICS12),
         NAICS_2 = ifelse(nchar(NAICS12) >= 2, substr(NAICS12, 1, 2), ""),
         NAICS_3 = ifelse(nchar(NAICS12) >= 3, substr(NAICS12, 1, 3), ""),
         NAICS_4 = ifelse(nchar(NAICS12) >= 4, substr(NAICS12, 1, 4), ""),)

temp_2 = subset(ILPA, nchar(NAICS12) == 2)
temp_3 = subset(ILPA, nchar(NAICS12) == 3)
temp_4 = subset(ILPA, nchar(NAICS12) == 4)

summary_naics_crosswalk <- BEA_NAICS_crosswalk_provided %>% select(c('Summary', 'NAICS_2', 'NAICS_3', 'NAICS_4')) %>% distinct()
temp_2 <- merge(temp_2, summary_naics_crosswalk, by = "NAICS_2")
temp_2 <- temp_2 %>% select(c('Summary', 'Industry Description')) %>% distinct()
temp_3 <- merge(temp_3, summary_naics_crosswalk, by = "NAICS_3")
temp_3 <- temp_3 %>% select(c('Summary', 'Industry Description')) %>% distinct()
temp_4 <- merge(temp_4, summary_naics_crosswalk, by = "NAICS_4")
temp_4 <- temp_4 %>% select(c('Summary', 'Industry Description')) %>% distinct()
ILPA <- rbind(temp_2, temp_3, temp_4)
ILPA$i <- ILPA$Summary
BEA_BLS_crosswalk <- merge(ILPA, code_desc_crosswalk, by = "i", all.y = TRUE)
BEA_BLS_crosswalk$`Industry Description (BLS)` <- BEA_BLS_crosswalk$`Industry Description`
BEA_BLS_crosswalk <- BEA_BLS_crosswalk %>% select(c('i', 'Industry Description (BLS)'))

# row sort by Code (important)
BEA_BLS_crosswalk <- BEA_BLS_crosswalk[order(BEA_BLS_crosswalk$i),]
tfp_KLEMS$`Industry Description (BLS)` <- tfp_KLEMS$Industry
tfp_KLEMS <- subset(tfp_KLEMS, !is.na(tfp_KLEMS$`Industry Description (BLS)`) & Measure == "Total factor productivity" & Units == "Index (2017=100)")
tfp_KLEMS <- merge(BEA_BLS_crosswalk, tfp_KLEMS, by = "Industry Description (BLS)", all.x = TRUE) # merge one-to-one; keep only ILPA value, for VCOV
tfp_KLEMS <- tfp_KLEMS[, c("i", 1987:2023)] 

# reshape long; the column names correspond to years, the values in the cells are TFP
tfp_KLEMS_long <- reshape2::melt(tfp_KLEMS, id.vars = "i", variable.name = "year", value.name = "TFP")
tfp_KLEMS_long$TFP <- as.numeric(tfp_KLEMS_long$TFP)

# merge with keep industries
industry_TFP <- tfp_KLEMS_long %>% select(i, year, TFP) 
industry_TFP <- merge(code_desc_crosswalk, industry_TFP, by = "i")
# generate lags, 1yr and 4yr (use dplyr, order by year)
industry_TFP <- tfp_KLEMS_long %>% arrange(i, year) %>%
                                  group_by(i) %>% 
                                  mutate(delta_tfp_1 = log(TFP) - log(lag(TFP)),
                                         delta_tfp_4 = log(TFP) - log(lag(TFP, 4))) 
industry_TFP$year <- as.numeric(as.character(industry_TFP$year))
industry_TFP <- subset(industry_TFP, year > 1997)

# save cleaned TFP data
save(industry_TFP, file = "clean_data/industry_TFP.RData")

# Clean Atalay data ===============================

# construct customer Leontief inverse
total_output_jt <- analysis_data %>%
  select(t, j = i, total_output_j = total_output) %>%
  distinct()

l_cust_data <- analysis_data %>%
  select(t, i, j, exp_ijt) %>%
  left_join(total_output_jt, by = c("t", "j")) %>%
  # a_cust_ijt = exp_ijt / total_output_j
  group_by(t, j) %>%
  mutate(a_cust_ijt = exp_ijt / total_output_j) %>%
  ungroup() %>%
  # compute Leontief inverse year by year
  group_by(t) %>%
  group_modify(~ {
    # A_cust is j x i matrix
    A_wide <- dcast(as.data.table(.x), j ~ i, value.var = "a_cust_ijt", fill = 0)
    industries <- A_wide$j
    A_mat <- as.matrix(A_wide[, -1, with = FALSE])
    rownames(A_mat) <- industries
    A_mat <- A_mat[industries, industries]
    # Leontief inverse
    L_mat <- solve(diag(nrow(A_mat)) - A_mat)
    # reshape long: rows are j, columns are i
    L_dt <- as.data.table(L_mat, keep.rownames = "j")
    L_long <- melt(L_dt, id.vars = "j", variable.name = "i", value.name = "l_cust_ijt")
    as.data.frame(L_long)
  }) %>%
  ungroup()

# join with defense spending data to construct Atalay instruments

defense_share_it <- analysis_data %>%
  ungroup() %>%
  select(i, t, defense_spending, total_output) %>%
  mutate(defense_share_it = defense_spending / total_output) %>%
  select(i, t, defense_share_it) %>%
  distinct()

atalay_iv_eq14 <- l_cust_data %>%
  left_join(defense_share_it, by = c("i" = "i", "t" = "t"), relationship = "many-to-one")  %>%
  left_join(FRED, by = c("t" = "year"), relationship = "many-to-one") %>%
  mutate(
    defense_share_it = ifelse(is.na(defense_share_it), 0, defense_share_it) # replace NAs with 0
  ) %>%
  ungroup() %>%
  group_by(j, t) %>%
  summarise(
    atalay_iv_eq14 = sum(l_cust_ijt * defense_share_it * delta_logFDEFX), 
    .groups = "drop"
  ) %>%
  filter(t >= 1998 & t <= 2023)

# join with filtered analysis data to estimate elasticity 

atalay_data <- merge(analysis_data_filtered, atalay_iv_eq14, by = c("j", "t"))
atalay_data <- atalay_data %>%
  group_by(i, t) %>%
  mutate(
    atalay_iv_eq16 = sum(atalay_iv_eq14 * a_ijt)
  )
atalay_data$rel_price_change <- atalay_data$delta_q_idt - atalay_data$delta_p_jdt 

save(atalay_data, file = "clean_data/atalay_data.RData")
