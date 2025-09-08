# load libraries
library(readxl)

# clean IO as share of gross revenue --------------------------------------

# define function taking pathname, sheetname, and returning make or use table
load_raw <- function(path, sheet){
  
  # load data and subset to relevant portion of sheet
  raw <- read_excel(path, sheet = sheet)
  column_names <- raw[6,]
  row_names <- raw[,1]
  raw <- raw[-c(1:6),]
  colnames(raw) <- column_names
  # convert columns 3 on to numeric
  raw[,3:ncol(raw)] <- lapply(raw[,3:ncol(raw)], as.numeric)
  # replace NAs with 0s
  raw[is.na(raw)] <- 0
  
  return(raw)
}

# take make table, return share table
make_to_share <- function(make){
  total <- as.numeric(make[make$`Industry Description` == "Total Commodity Output",])
  make <- make[1:43,1:45] # exclude government portion and totals
  total <- total[1:45] # exclude government portion and totals
  sd <- make
  sd[, 3:ncol(sd)] <- sweep(sd[, 3:ncol(sd)], 2, total[3:ncol(sd)] , FUN = "/")
  return(sd)
}

# take use table, return directrequirements table
use_to_dr <- function(use){
  total <- as.numeric(use[use$`Commodity Description` == "Total Industry Output",]) # I normalize by industry output here
  use <- use[1:43, 1:45] # exclude government portion and totals
  total <- total[1:45] # exclude government portion and totals
  dr <- use
  dr[, 3:ncol(dr)] <- sweep(dr[, 3:ncol(dr)], 2, total[3:ncol(dr)], FUN = "/")
  return(dr)
}

# take share and dr table, return A, input-output by industry
share_dr_to_A <- function(share, dr){
  labels <- share[,1:2]
  # row i is shares of commodities c produced by industry i (share is industry-by-commodity)
  # column j is spending by industry j on commodities c (use is commodity-by-industry)
  # ij matrix result is expenditure ji, i.e. expenditure by column industry on row industry
  A_prime <- as.matrix(share[,3:ncol(share)]) %*% as.matrix(dr[,3:ncol(dr)]) # DR' X S' = (S X DR)'
  A <- t(A_prime)
  colnames(A) <- t(labels[,1])
  A <- cbind(labels, A)
  return(A)
}

wrapper <- function(year) {
  use <- load_raw(paste0("data/raw/IOTables_1947-1996/IOUse_Before_Redefinitions_PRO_1947-1962_Summary.xlsx"), year)
  make <- load_raw(paste0("data/raw/IOTables_1947-1996/IOMake_Before_Redefinitions_1947-1962_Summary.xlsx"), year)
  share <- make_to_share(make)
  dr <- use_to_dr(use)
  A <- share_dr_to_A(share, dr)
  total_industry_output <- make[, ncol(make)]
  A <- cbind(A, total_industry_output[1:43,])
  return(A)
}

# create vector of years, 1947-1962, as strings
years <- as.character(1947:1962)

# create list of A matrices
A_byyear_1947_1962 <- lapply(years, wrapper)

# label each A matrix with year
names(A_byyear_1947_1962) <- years



# generate A as share of int. input spending  ------------------------------------------

# gen_A_int
gen_A_int <- function(A_byyear) {
  A_Int_byyear <- A_byyear
  for (i in 1:length(A_byyear)) {
    exp <- A_byyear[[i]][,3:(ncol(A_byyear[[i]])-1)] * A_byyear[[i]][,ncol(A_byyear[[i]])]
    rowsums <- rowSums(exp)
    # divide each row by rowsum
    int_exp <- sweep(exp, 1, rowsums, FUN = "/")
    A_Int_byyear[[i]][,3:(ncol(A_Int_byyear[[i]])-1)] <- int_exp
  }
  
  return(A_Int_byyear)
  
}

A_Int_byyear_1947_1962 = gen_A_int(A_byyear_1947_1962)

# save A matrices
save(A_byyear_1947_1962, file = "data/cleaned/A_byyear_1947-1962.RData")
save(A_Int_byyear_1947_1962, file = "data/cleaned/A_Int_byyear_1947_1962.RData")
