# libraries
library(reshape2)
library(data.table)
library(dplyr)
library(readr)
setwd('/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/het_sectoral_prod/V2')
# load data
load("raw_data/BEA_supply&use.RData")

# define the 66 non-govt industries we care about
# Also corresponds to 66 non-govt commodity codes
non_govt_industries <- unique(supply_data_raw$ColCode) %>%
  # filter out T codes, G codes, and Trade + Trans + Imports + adjustments
  .[!grepl("^T", .) & !grepl("^G", .) & !grepl("^M", .) &  . != "Trade" & . != "Trans" & . != "SUB"]

# extract personal consumption expenditures by commodity
pce_commodity <- use_data_raw %>%
  filter(
    RowCode %in% non_govt_industries & ColCode == "F010"
  ) %>%
  group_by(Year) %>%
  mutate(
    commodity = RowCode,
    pce = as.numeric(DataValue)
  ) %>%
  select(Year, commodity, pce)

# extract defense spending by commodity
defense_commodity <- use_data_raw %>%
  filter(
    RowCode %in% non_govt_industries & ColCode == "GFGD"
  ) %>%
  group_by(Year) %>%
  mutate(
    commodity = RowCode,
    defense_spending = as.numeric(DataValue)
  ) %>%
  select(Year, commodity, defense_spending)

# extract labor compensation by industry
labor_comp_industry <- use_data_raw %>%
  filter(
    ColCode %in% non_govt_industries & RowCode == "V001"
  ) %>%
  mutate(
    industry = ColCode,
    labor_comp = as.numeric(DataValue)
  ) %>%
  select(Year, industry, labor_comp)

# extract total industry output by industry (for later use)
total_industry <- use_data_raw %>%
  filter(
    ColCode %in% non_govt_industries & RowCode == "T018"
  ) %>%
  mutate(
    industry = ColCode,
    total_output = as.numeric(DataValue)
  ) %>%
  select(Year, industry, total_output)

# filter to only look at commodities that are non-govt industry
# and industries that are the same + imports + total supply
use_data_raw <- use_data_raw %>%
  filter(
    RowCode %in% non_govt_industries & ColCode %in% c(non_govt_industries)
  )  
supply_data_raw <- supply_data_raw %>%
  filter(
    RowCode %in% non_govt_industries & ColCode %in% c(non_govt_industries, "T007", "T013", "MCIF")
  )


# 1) get sectoral IO values (+ sectoral import ratios)  ---------------------------------

# get import ratio for commodities
import_ratio_commodity <- supply_data_raw %>%
  mutate(
    commodity = RowCode
  ) %>%
  group_by(commodity, Year) %>%
  reframe(
    import = DataValue[ColCode == "MCIF"], # MCIF is Imports
    total_supply = DataValue[ColCode == "T013"], # T013 is Total commodity supply (basic prices)
    import_ratio = as.numeric(import) / as.numeric(total_supply) # import ratio
  ) %>%
  select(commodity, Year, import_ratio)

# construct sectoral goods as weights of commodities
sectoral_goods_weights <- supply_data_raw %>%
  mutate(
    commodity = RowCode,
    industry_supply = ColCode
  ) %>%
  group_by(commodity, Year) %>%
  mutate(
    domestic_supply = as.numeric(DataValue[ColCode == "T007"]) # T007 is Total Commodity Output (basic prices)
  ) %>%
  group_by(commodity, Year) %>%
  mutate(
    sectoral_goods_weights = as.numeric(DataValue) / domestic_supply,
    commodity = RowCode,
    industry = ColCode
  ) %>%
  filter(industry_supply %in% non_govt_industries) %>%
  select(Year, industry_supply, commodity, sectoral_goods_weights)

# get import ratio for sectoral goods
import_ratio_sectorgoods <- sectoral_goods_weights %>%
  left_join(
    import_ratio_commodity, 
    by = c("commodity" = "commodity", "Year" = "Year"),
    relationship = "many-to-one") %>%
  mutate(
    import_ratio = ifelse(is.na(import_ratio), 0, import_ratio), # set NA import ratios to 0
    weighted_import_ratio = sectoral_goods_weights * import_ratio
  ) %>%
  group_by(industry_supply, Year) %>%
  reframe(
    import_ratio = sum(weighted_import_ratio)
  ) %>% 
  group_by(industry_supply) %>%
  mutate(
    mean_import_ratio = mean(import_ratio),
    import_ratio = ifelse(mean_import_ratio < 0.25, 0, import_ratio) # for sectors that have very low import ratios on average, set to 0 and deem non-tradeable
  ) %>%
  select(industry_supply, Year, import_ratio)

# join with foreign_domestic_use to get industry-industry use values
industry_industry_exp <- use_data_raw %>%
  mutate(
    commodity = RowCode,
    industry_use = ColCode,
    use = as.numeric(DataValue)
  ) %>%
  # will be many-to-many join
  left_join(
    sectoral_goods_weights,
    relationship = "many-to-many",
    by = c("commodity" = "commodity", "Year" = "Year")
  ) %>%
  group_by(industry_use, industry_supply, Year) %>%
  summarize(
    exp = sum(use * sectoral_goods_weights)
  ) %>%
  left_join(
    import_ratio_sectorgoods,
    by = c("industry_supply" = "industry_supply", "Year" = "Year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    s_ijdt = 1 - import_ratio,
    exp_ijdt = exp * s_ijdt,
    exp_ijt = exp
  ) %>%
  left_join(
    labor_comp_industry,
    by = c("industry_use" = "industry", "Year" = "Year"),
    relationship = "many-to-one"
  ) %>%
  select(industry_use, industry_supply, Year, exp_ijt, exp_ijdt, s_ijdt, labor_comp) 

# recover personal consumption expenditures by industry
pce_industry <- pce_commodity %>%
  left_join(
    sectoral_goods_weights,
    by = c("commodity" = "commodity", "Year" = "Year"),
    relationship = "one-to-many"
  ) %>%
  group_by(industry_supply, Year) %>%
  summarize(
    pce = sum(pce * sectoral_goods_weights)
  ) %>%
  group_by(Year) %>%
  mutate(
    i = industry_supply,
    total_pce = sum(pce),
    pce_share = pce / total_pce
  ) %>%
  select(i, Year, pce, pce_share)

# recover defense spending by industry
defense_industry <- defense_commodity %>%
  left_join(
    sectoral_goods_weights,
    by = c("commodity" = "commodity", "Year" = "Year"),
    relationship = "one-to-many"
  ) %>%
  group_by(industry_supply, Year) %>%
  summarize(
    defense_spending = sum(defense_spending * sectoral_goods_weights)
  ) %>%
  group_by(Year) %>%
  mutate(
    i = industry_supply,
    defense_spending = sum(defense_spending)
  ) %>%
  select(i, Year, defense_spending)

# add domestic industry prices --------------------------------------------

load("raw_data/BEA_price.RData")
price_data <- output_price_data_raw %>%
  select(Industry, Year, DataValue) %>%
  mutate(
    industry = Industry,
    price = as.numeric(DataValue)
  ) %>%
  select(industry, Year, price)

ii_price_data <- II_price_data_raw %>%
  select(Industry, Year, DataValue) %>%
  mutate(
    industry = Industry,
    ii_price = as.numeric(DataValue)
  ) %>%
  select(industry, Year, ii_price)

# combine, and compute relevant expenditure shares, for final analysis dataset
analysis_data <- industry_industry_exp %>%
  # join in prices for industry_use
  left_join(
    price_data,
    by = c("industry_supply" = "industry", "Year" = "Year") # care about supply prices
  ) %>%
  left_join(
    ii_price_data,
    by = c("industry_use" = "industry", "Year" = "Year") # care about use II prices
  ) %>%
  group_by(industry_use, Year) %>%
  mutate(
    i = industry_use,
    j = industry_supply,
    t = Year,
    alpha_it = labor_comp/(labor_comp + sum(exp_ijt)),
    a_ijt = exp_ijt / sum(exp_ijt),
    a_ijdt = exp_ijdt / sum(exp_ijt),
    p_jdt = price,
    q_idt = ii_price,
    log_a_ijdt = log(a_ijdt),
    log_s_ijdt = log(s_ijdt),
    log_p_jdt = log(p_jdt),
    log_q_idt = log(q_idt)
  ) %>%
  # first difference to get changes (logs)
  arrange(i, j, t) %>%
  group_by(i, j) %>%
  mutate(
    delta_a_ijdt = log_a_ijdt - lag(log_a_ijdt),
    delta_s_ijdt = log_s_ijdt - lag(log_s_ijdt),
    delta_p_jdt = log_p_jdt - lag(log_p_jdt),
    delta_q_idt = log_q_idt - lag(log_q_idt)
  ) %>%
  select(
    i,
    j,
    t,
    exp_ijt,
    labor_comp,
    alpha_it,
    a_ijt,
    a_ijdt,
    s_ijdt,
    p_jdt,
    q_idt,
    log_a_ijdt,
    log_s_ijdt,
    log_p_jdt,
    log_q_idt,
    delta_a_ijdt,
    delta_s_ijdt,
    delta_p_jdt, 
    delta_q_idt
  ) %>%
  left_join(
    pce_industry %>% select(i, t = Year, pce_share),
    by = c("i" = "i", "t" = "t"),
    relationship = "many-to-one"
  ) %>%
  rename(
    beta_it = pce_share
  ) %>%
  left_join(
    defense_industry %>% select(i, t = Year, defense_spending),
    by = c("i" = "i", "t" = "t"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    total_industry %>% select(industry = industry, t = Year, total_output),
    by = c("i" = "industry", "t" = "t"),
    relationship = "many-to-one"
  )

# Non-negative expenditures, years we see changes, and no self-loops
analysis_data_filtered <- analysis_data %>%
  mutate(
    delta_a_ijdt = ifelse(is.infinite(delta_a_ijdt), NA, delta_a_ijdt),
    delta_s_ijdt = ifelse(is.infinite(delta_s_ijdt), NA, delta_s_ijdt),
    delta_p_jdt = ifelse(is.infinite(delta_p_jdt), NA, delta_p_jdt)
  ) %>%
  filter(t >= 1998, a_ijt > 0, a_ijdt > 0, s_ijdt > 0, # i != j,
         !is.na(delta_a_ijdt), !is.na(delta_s_ijdt), !is.na(delta_p_jdt)) %>%
  group_by(j) %>%
  mutate(
    mean_s_ijdt = mean(s_ijdt),
    tradeable = ifelse(mean_s_ijdt < 1, 1, 0) # for interaction term later
  ) %>%
  group_by(i, j) %>%
  mutate(
    mean_ij = mean(a_ijt)
  ) %>%
  group_by(i) %>%
  mutate(
    rank_ij = dense_rank(desc(mean_ij))  # highest mean_ij gets rank 1
  ) %>% 
  # filter(rank_ij <= 5)
  filter(mean_ij >= 1e-2) # keep only >1% suppliers


# save final analysis data
save(analysis_data, analysis_data_filtered, file = "clean_data/analysis_data.RData")
# write csv for python
write.csv(analysis_data, file = "clean_data/analysis_data.csv", row.names = FALSE)
write.csv(analysis_data_filtered, file = "clean_data/analysis_data_filtered.csv", row.names = FALSE)
