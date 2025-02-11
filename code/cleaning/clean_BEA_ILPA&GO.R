# load libraries 
library(readxl)
library(dplyr)
library(zoo)
library(stringr)
library(tidyr)

# make sure lag references dplyr command (IMPORTANT)
lag <- dplyr::lag

# load libraries
load("data/cleaned/code_desc_crosswalk.RData")
load("data/raw/api_pull_BEA/GrossOutput_ValueAdded.RData")
load("data/cleaned/BEA_NAICS_crosswalk_provided.RData")
keep = code_desc_crosswalk[1:66,]
keep <- keep[order(keep$Code),] # sort by Code
 

# BLS/BEA crosswalk ------------------------------------------------------
# annoyingly Descriptions in KLEMS TFP data from BLS don't perfectly match BEA; slightly more aggregated
# crosswalk with NAICS codes using the joint BEA/ILPA sheet, which has NAICS/Descriptions for official BLS TFP
ILPA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
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
ILPA$Code <- ILPA$Summary
keep <- code_desc_crosswalk[1:66,c("Code")]
BEA_BLS_crosswalk <- merge(ILPA, keep, by = "Code", all.y = TRUE)
BEA_BLS_crosswalk$`Industry Description (BLS)` <- BEA_BLS_crosswalk$`Industry Description`
BEA_BLS_crosswalk <- BEA_BLS_crosswalk %>% select(c('Code', 'Industry Description (BLS)'))
# row sort by Code (important)
BEA_BLS_crosswalk <- BEA_BLS_crosswalk[order(BEA_BLS_crosswalk$Code),]

# TFP BEA data ------------------------------------------------------------

tfp_BEA <- read_excel("data/raw/misc/major-industry-total-factor-productivity-klems.xlsx", 
                      sheet = "Annual", skip = 2)
tfp_BEA$`Industry Description (BLS)` <- tfp_BEA$Industry
tfp_BEA <- subset(tfp_BEA, !is.na(tfp_BEA$`Industry Description (BLS)`) & Measure == "Total factor productivity" & Units == "Index (2017=100)")
tfp_BEA <- merge(BEA_BLS_crosswalk, tfp_BEA, by = "Industry Description (BLS)", all.x = TRUE) # merge one-to-one; keep only ILPA value, for VCOV

tfp_BEA <- tfp_BEA[, c("Code", 1987:2023)] 

# reshape long; the column names correspond to years, the values in the cells are TFP
tfp_BEA_long <- reshape2::melt(tfp_BEA, id.vars = "Code", variable.name = "year", value.name = "TFP")
tfp_BEA_long$TFP <- as.numeric(tfp_BEA_long$TFP)

# Real GO BEA data --------------------------------------------------------

real_gross_output <- real_gross_output_data_raw 
real_gross_output$year <- as.numeric(real_gross_output$Year)
real_gross_output$Code <- real_gross_output$Industry 
real_gross_output$real_gross_output <- as.numeric(real_gross_output$DataValue)
real_gross_output <- merge(real_gross_output, code_desc_crosswalk, by = "Code")
real_gross_output <- real_gross_output[, c("Code", "year", "real_gross_output")]
real_gross_output <- real_gross_output %>% 
  arrange(Code, year) %>%
  group_by(Code) %>%
  mutate(
    delta_logGO_1 = log(real_gross_output) - log(lag(real_gross_output, 1))
  )

real_GO_long <- real_gross_output


# nominal GO/VA BEA data -----------------------------------------------------

gross_output <- gross_output_data_raw
gross_output$Code <- gross_output$Industry
gross_output$year <- as.numeric(gross_output$Year)
gross_output$GrossY <- as.numeric(gross_output$DataValue)
GO_long <- gross_output %>% select(Code, year, GrossY)
# generate log-change year-over-year in Gross Output
GO_long <- GO_long %>% 
  arrange(Code, year) %>%
  group_by(Code) %>%
  mutate(
    delta_logGO_1 = log(GrossY) - log(lag(GrossY, 1))
  )

value_added <- value_added_raw
value_added$Code <- value_added$Industry
value_added$year <- as.numeric(value_added$Year)
value_added$VAdd <- as.numeric(value_added$DataValue)
VA_long <- value_added %>% select(Code, year, VAdd)

# save --------------------------------------------------------------------

save(tfp_BEA_long, real_GO_long, GO_long, VA_long, file = "data/cleaned/BEA_ILPAGO_data.RData")

# OLD CODE BELOW ----------------------------------------------------------

# 
# # load BEA/BLS compensation data --------------------------------------------------
# 
# # load R&D BEA data (compensation)
# rd_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                      sheet = "Capital_R&D Compensation", skip = 1)
# rd_BEA <- subset(rd_BEA, !is.na(rd_BEA$`Industry Description`))
# rd_BEA <- merge(rd_BEA, code_desc_crosswalk, by = "Industry Description")
# rd_BEA <- rd_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are R&D
# rd_BEA_long <- reshape2::melt(rd_BEA, id.vars = "Code", variable.name = "year", value.name = "rd")
# rd_BEA_long$rd = rd_BEA_long$rd * 10e6 # number is millions
# rd_BEA_long$log_rd <- log(rd_BEA_long$rd + 1) # + 1 trick for 0s
# # generate lag and rolling averages (in billions)
# rd_BEA_long <- rd_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     rd_1 = lag(rd, 1)/10e9,
#     rd_5 = lag(rd, 5)/10e9,
#     rd_10 = lag(rd, 10)/10e9,
#     rd_1_ave = rollapply(rd, width = 1, FUN = mean, align = "right", fill = NA)/10e9,
#     rd_5_ave = rollapply(rd, width = 5, FUN = mean, align = "right", fill = NA)/10e9,
#     rd_10_ave = rollapply(rd, width = 10, FUN = mean, align = "right", fill = NA)/10e9,
#   )
# 
# # load BEAL/BLS quantity data ---------------------------------------------
# 
# # load Labor Input Quantity BEA
# labor_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                         sheet = "Labor Input_Quantity", skip = 1)
# labor_BEA <- subset(labor_BEA, !is.na(labor_BEA$`Industry Description`))
# labor_BEA <- merge(labor_BEA, code_desc_crosswalk, by = "Industry Description")
# labor_BEA <- labor_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are labor inputs
# labor_BEA_long <- reshape2::melt(labor_BEA, id.vars = "Code", variable.name = "year", value.name = "labor")
# # generate percent change in labor inputs
# labor_BEA_long <- labor_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_labor_1` = log(labor) - log(lag(labor, 1)),
#     `delta_labor_5` = log(labor) - log(lag(labor, 5)),
#     `delta_labor_10` = log(labor) - log(lag(labor, 10)))
# 
# # load Energy Inputs BEA data (quantity)
# energy_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                          sheet = "Energy_Quantity", skip = 1)
# energy_BEA <- subset(energy_BEA, !is.na(energy_BEA$`Industry Description`))
# energy_BEA <- merge(energy_BEA, code_desc_crosswalk, by = "Industry Description")
# energy_BEA <- energy_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are energy inputs
# energy_BEA_long <- reshape2::melt(energy_BEA, id.vars = "Code", variable.name = "year", value.name = "energy")
# # generate percent change in energy inputs
# energy_BEA_long <- energy_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_energy_1` = log(energy) - log(lag(energy, 1)),
#     `delta_energy_5` = log(energy) - log(lag(energy, 5)),
#     `delta_energy_10` = log(energy) - log(lag(energy, 10)))
# 
# # load RD BEA data (quantity)
# rdq_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                       sheet = "Capital_R&D_Quantity", skip = 1)
# rdq_BEA <- subset(rdq_BEA, !is.na(rdq_BEA$`Industry Description`))
# rdq_BEA <- merge(rdq_BEA, code_desc_crosswalk, by = "Industry Description")
# rdq_BEA <- rdq_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are R&D
# rdq_BEA_long <- reshape2::melt(rdq_BEA, id.vars = "Code", variable.name = "year", value.name = "rdq")
# # generate percent change in R&D
# rdq_BEA_long <- rdq_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_rdq_1` = log(rdq) - log(lag(rdq, 1)),
#     `delta_rdq_5` = log(rdq) - log(lag(rdq, 5)),
#     `delta_rdq_10` = log(rdq) - log(lag(rdq, 10)), 
#     `delta_rdq_1_alt` = lag(delta_rdq_1, 1),
#     `delta_rdq_5_alt` = lag(delta_rdq_1, 5),
#     `delta_rdq_10_alt` = lag(delta_rdq_1, 10))
# 
# # load Software BEA data (quantity)
# software_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                            sheet = "Capital_Software_Quantity", skip = 1)
# software_BEA <- subset(software_BEA, !is.na(software_BEA$`Industry Description`))
# software_BEA <- merge(software_BEA, code_desc_crosswalk, by = "Industry Description")
# software_BEA <- software_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are software
# software_BEA_long <- reshape2::melt(software_BEA, id.vars = "Code", variable.name = "year", value.name = "software")
# # generate percent change in software
# software_BEA_long <- software_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_software_1` = log(software) - log(lag(software, 1)),
#     `delta_software_5` = log(software) - log(lag(software, 5)),
#     `delta_software_10` = log(software) - log(lag(software, 10)))
# 
# # load IT BEA data (quantity)
# it_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                      sheet = "Capital_IT_Quantity", skip = 1)
# it_BEA <- subset(it_BEA, !is.na(it_BEA$`Industry Description`))
# it_BEA <- merge(it_BEA, code_desc_crosswalk, by = "Industry Description")
# it_BEA <- it_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are IT
# it_BEA_long <- reshape2::melt(it_BEA, id.vars = "Code", variable.name = "year", value.name = "it")
# # generate percent change in IT
# it_BEA_long <- it_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_it_1` = log(it) - log(lag(it, 1)),
#     `delta_it_5` = log(it) - log(lag(it, 5)),
#     `delta_it_10` = log(it) - log(lag(it, 10)))
# 
# # load Comp Quantity BEA data
# compq_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-Expanded-Capital-Detail-1987-2021.xlsx", 
#                         sheet = "Capital__COMP_Quantity", skip = 1)
# compq_BEA <- subset(compq_BEA, !is.na(compq_BEA$`Industry Description`))
# compq_BEA <- merge(compq_BEA, code_desc_crosswalk, by = "Industry Description")
# compq_BEA <- compq_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are Comp
# compq_BEA_long <- reshape2::melt(compq_BEA, id.vars = "Code", variable.name = "year", value.name = "comp")
# # generate percent change in Comp
# compq_BEA_long <- compq_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_compq_1` = log(comp) - log(lag(comp, 1)),
#     `delta_compq_5` = log(comp) - log(lag(comp, 5)),
#     `delta_compq_10` = log(comp) - log(lag(comp, 10)))
# 
# 
# # generate 5 year percent change in TFP
# tfp_BEA_long <- tfp_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_tfp_1` = log(TFP) - log(lag(TFP, 1)),
#     `delta_tfp_5` = log(TFP) - log(lag(TFP, 5)),
#     `delta_tfp_10` = log(TFP) - log(lag(TFP, 10)))
# 
# # load Integrated Labor Productivity data
# lp_BEA <- read_excel("data/raw/BEA_ILPA/BEA-BLS-industry-level-production-account-1987-2021.xlsx", 
#                      sheet = "Integrated Labor Productivity", skip = 1)
# lp_BEA <- subset(lp_BEA, !is.na(lp_BEA$`Industry Description`))
# lp_BEA <- merge(lp_BEA, code_desc_crosswalk, by = "Industry Description")
# lp_BEA <- lp_BEA[, c("Code", 1987:2021)]
# 
# # reshape long; the column names correspond to years, the values in the cells are LP
# lp_BEA_long <- reshape2::melt(lp_BEA, id.vars = "Code", variable.name = "year", value.name = "LP")
# # generate 5 year percent change in LP
# lp_BEA_long <- lp_BEA_long %>%
#   arrange(`Code`, year) %>%
#   group_by(`Code`) %>%
#   mutate(
#     `delta_lp_1` = log(LP) - log(lag(LP, 1)),
#     `delta_lp_5` = log(LP) - log(lag(LP, 5)),
#     `delta_lp_10` = log(LP) - log(lag(LP, 10)))
# 
# 
# # merge all industry-level data ---------------------------------------------------------------
# 
# analysis_data <- tfp_BEA_long
# analysis_data <- merge(analysis_data, rd_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, energy_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, rdq_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, software_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, it_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, compq_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, lp_BEA_long, by = c("Code", "year"), all.x = TRUE)
# analysis_data <- merge(analysis_data, labor_BEA_long, by = c("Code", "year"), all.x = TRUE)
# BEA_ILPA_data <- analysis_data

# save data ---------------------------------------------------------------
# save(BEA_ILPA_data, file = "data/cleaned/BEA_ILPA_data.RData")