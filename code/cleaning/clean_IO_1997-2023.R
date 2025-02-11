# libraries
library(reshape2)
library(data.table)
# load data
load("data/cleaned/code_desc_crosswalk.RData")
load("data/raw/api_pull_BEA/make&use.RData")


# Make grid of non-govt industries/commodities ----------------------------

load("data/cleaned/code_desc_crosswalk.RData")
exclude = code_desc_crosswalk[67:nrow(code_desc_crosswalk), ] 
keep = subset(code_desc_crosswalk, !(Code %in% exclude$Code))
commodity_vals = keep$Code 
industry_vals = keep$Code
year = 1997:2023
keep_grid = expand.grid(commodity = commodity_vals, industry = industry_vals, year = year) # this is the grid of Industry/Commodity codes we use in the analysis

# Clean make&use data, long -----------------------------------------------

# Clean make data (supply of commodities by industry, $) 
# and use data (use of commodities by industry, $)
# can see in raw data that both make and use dataframes have RowType=Commodity, ColType=Industry
make_data <- make_data_raw
make_data <- data.frame(make_data$RowCode, make_data$ColCode, make_data$Year, make_data$DataValue)
colnames(make_data) = c("commodity", "industry", "year", "val")
make_data$val = as.numeric(make_data$val)
use_data <- use_data_raw
use_data <- data.frame(use_data$RowCode, use_data$ColCode, use_data$Year, use_data$DataValue)
colnames(use_data) = c("commodity", "industry", "year", "val")
use_data$val = as.numeric(use_data$val)

# Extract total commodity output values 
# We use this to normalize make_data values; they now represent % of commodity output attributable to each industry
total_commodity <- make_data[make_data$industry == "T007", c("year", "commodity", "val")] # T007 is the total commodity output
colnames(total_commodity) <- c("year", "commodity", "total_commodity_output")
make_data <- merge(make_data, total_commodity, by = c("year", "commodity"))
make_data$total_commodity_output = make_data$total_commodity_output
make_data$val = make_data$val / make_data$total_commodity_output # normalize 
make_data <- make_data[, c("commodity", "industry", "year", "val")]

# Extract A) Total industry output and B) Total intermediate expenditure year
# Later on we will use this to normalize Industry Use tables (i.e. convert to % of total industry output/intermediate expenditure)
total_industry <- use_data[use_data$commodity == "T018", c("year", "industry", "val")] # T018 is Total industry output (basic prices)
colnames(total_industry) <- c("year", "industry", "total_industry_output")
total_int <- use_data[use_data$commodity == "T005", c("year", "industry", "val")] # T005 is Total Intermediate
colnames(total_int) <- c("year", "industry", "total_int_output")

# Extract spending by GFGD (Govt, General Defense) on commodities
# Later on we will use this to construct the Atalay instruments
defense_use <- use_data[use_data$industry == "GFGD", c("commodity", "industry", "year", "val")] # T011 is defense spending

# Merge with grid, so we have only the 66 non-govt industries/commodities
make_data <- merge(keep_grid, make_data, by = c("commodity", "industry", "year"), all.x = TRUE)
use_data <- merge(keep_grid, use_data, by = c("commodity", "industry", "year"), all.x = TRUE)
defense_use <- merge(expand.grid(commodity=commodity_vals, year=year), defense_use, by = c("commodity", "year"), all.x = TRUE)
defense_use$industry <- "GFGD" # set this for all rows

# Reshape wide preparing for matrix multiply ----------------------------------------

# Split long dataframe into list of dataframes by year
# Need to do this to reshape wide, creating matrices used for Industry-IO tables
make_data_split <- split(make_data, make_data$year)
use_data_split <- split(use_data, use_data$year)
defense_use_split <- split(defense_use, defense_use$year)

# Make = commodity x industry matrix
# Make_ci = (P_c X_cj)/(P_c Y_c) = % of commodity output attributable to each industry
make_data_list <- lapply(make_data_split, function(data) {
  
  # for future readers: this is so god damn convoluted because originally I kept things as a dataframe
  # but now R gives me a warning that dcast wants a data.table, so I have to convert it to a data.table
  # which unlike the dataframe dcast lists x as a column instead of rownames, as I treat it later on
  dat <- data.table(data)
  matrix_data <- dcast(dat, commodity ~ industry, value.var = "val")
  row_names <- matrix_data$commodity
  matrix_data <- as.matrix(matrix_data[, -1, with = FALSE])  # Remove the commodity column
  rownames(matrix_data) <- row_names
  
  matrix_data[is.na(matrix_data)] <- 0 # set NA to 0
  
  return(matrix_data)
})

# Use = industry x commodity matrix
# Use_ic = P_c X_ic = Total spending by industry i on commodity c
use_data_list <- lapply(use_data_split, function(data) {
  
  # for future readers: this is so god damn convoluted because originally I kept things as a dataframe
  # but now R gives me a warning that dcast wants a data.table, so I have to convert it to a data.table
  # which unlike the dataframe dcast lists x as a column instead of rownames, as I treat it later on
  dat <- data.table(data) 
  matrix_data <- dcast(dat, industry ~ commodity, value.var = "val")
  row_names <- matrix_data$industry
  matrix_data <- as.matrix(matrix_data[, -1])  # Remove the industry column
  rownames(matrix_data) <- row_names
  
  
  matrix_data[is.na(matrix_data)] <- 0 # set NA to 0
  
  return(matrix_data)
})

# Defense use = industry x commodity vector
# Defense_ic = P_c X_ic = Total spending by industry i (just GFGD) on commodity c
defense_use_list <- lapply(defense_use_split, function(data) {
  # for future readers: this is so god damn convoluted because originally I kept things as a dataframe
  # but now R gives me a warning that dcast wants a data.table, so I have to convert it to a data.table
  # which unlike the dataframe dcast lists x as a column instead of rownames, as I treat it later on
  dat <- data.table(data) # stupid add-in line, but keep getting warning on dcast if it's not using data.table
  matrix_data <- dcast(dat, industry ~ commodity, value.var = "val")
  row_names <- matrix_data$industry
  matrix_data <- as.matrix(matrix_data[, -1])  # Remove the industry column
  rownames(matrix_data) <- row_names
  
  matrix_data[is.na(matrix_data)] <- 0 # set NA to 0
  return(matrix_data)
})

# Name the list elements by year
names(make_data_list) <- names(make_data_split)
names(use_data_list) <- names(use_data_split)


# Matrix multiply to get IndustryXIndustry IO tables ----------------------

industry_by_industry_tables <- lapply(names(make_data_list), function(year) {
  make_data <- make_data_list[[year]]
  use_data <- use_data_list[[year]]
  defense_data <- defense_use_list[[year]]
  
  # Make IO tables
  # a_ij = sum_c P_c X_ic * (P_c X_cj)/(P_c Y_c) = P_j X_ij, i.e. spending by industry i on industry j 
  # Normalize by total (USING) industry output (A), total (USING) intermediate expenditure (A_int), and total (PRODUCING) industry output (A_customer)
  A <- use_data %*% make_data
  A <- as.data.frame(A)
  A$Code <- rownames(A)
  A$industry <- rownames(A)
  A$year <- year
  A <- merge(A, code_desc_crosswalk, by = "Code", all.x = TRUE)
  A_out <- merge(A, total_industry, by = c("industry", "year"))
  A_int <- merge(A, total_int, by = c("industry", "year"))
  A_out <- subset(A_out, select = -c(industry)) # industry = Code
  A_int <- subset(A_int, select = -c(industry)) # industry = Code
  
  # Normalize by industry output (A/A_customer)
  sub_matrix <- A_out[, !(names(A_out) %in% c("Code", "Industry Description", "year", "total_industry_output"))]
  A_out_divided <- sweep(sub_matrix, 1, A_out$total_industry_output, "/") # divide row-by-row, to get (P_j X_ij)/(P_i Y_i)
  A_out_divided_customers <- sweep(sub_matrix, 2, A_out$total_industry_output, "/") # divide column-by-column (P_j X_ij)/(P_j Y_j)
  colnames(A_out_divided_customers) <- colnames(sub_matrix)
  A_out <- cbind(A_out[, c("Code", "Industry Description")], A_out_divided, A_out["total_industry_output"])
  A_out_customers <- cbind(A_out[, c("Code", "Industry Description")], A_out_divided_customers, A_out["total_industry_output"])
  
  # Normalize by industry intermediate expenditure (A_int)
  sub_matrix_int <- A_int[, !(names(A_int) %in% c("Code", "Industry Description", "year", "total_int_output"))]
  row_total <- rowSums(sub_matrix_int)
  # A_int_divided <- sweep(sub_matrix_int, 1, A_int$total_int_output, "/") # divide row-by-row
  A_int_divided <- sweep(sub_matrix_int, 1, row_total, "/") # divide row-by-row
  A_int <- cbind(A_int[, c("Code", "Industry Description")], A_int_divided, A_int["total_int_output"])
  
  # Generate defense spending vector
  # d_i = sum_c P_c D_c * P_c X_ic/(P_c Y_c) = P_i D_i, i.e. Defense Spending on Industry i 
  # normalize by total industry output of i, share of gross revenues attributable to defense spending
  defense <- defense_data %*% make_data
  defense = defense/A_out$total_industry_output 

  # Return all tables in list
  result <- list(A_out, A_out_customers, A_int, defense)
  names(result) <- c("A_out", "A_out_customers", "A_int", "defense")
  return(result)
})

names(industry_by_industry_tables) <- names(make_data_list) # Name the list elements by year

# Extract and save tables ------------------------------------------------
A_byyear_1997_2023 <- lapply(industry_by_industry_tables, function(x) x$A_out)
A_Cust_byyear_1997_2023 <- lapply(industry_by_industry_tables, function(x) x$A_out_customers)
A_Int_byyear_1997_2023 <- lapply(industry_by_industry_tables, function(x) x$A_int)
pct_defense_byyear_1997_2023 <- lapply(industry_by_industry_tables, function(x) x$defense)
save(A_byyear_1997_2023, file = "data/cleaned/A_byyear_1997_2023.RData")
save(A_Int_byyear_1997_2023, file = "data/cleaned/A_Int_byyear_1997_2023.RData")
save(A_Cust_byyear_1997_2023, file = "data/cleaned/A_Cust_byyear_1997_2023.RData")
save(pct_defense_byyear_1997_2023, file = "data/cleaned/pct_defense_byyear_1997_2023.RData")





