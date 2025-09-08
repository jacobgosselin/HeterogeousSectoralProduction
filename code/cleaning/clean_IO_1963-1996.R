# load libraries
library(readxl)

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
  make <- make[1:61,1:63] # exclude government portion and totals
  total <- total[1:63] # exclude government portion and totals
  sd <- make
  sd[, 3:ncol(sd)] <- sweep(sd[, 3:ncol(sd)], 2, total[3:ncol(sd)] , FUN = "/")
  return(sd)
}

# take use table, return directrequirements table
use_to_dr <- function(use){
  total <- as.numeric(use[use$`Commodity Description` == "Total Industry Output",])
  use <- use[1:61, 1:63] # exclude government portion and totals
  total <- total[1:63] # exclude government portion and totals
  dr <- use
  dr[, 3:ncol(dr)] <- sweep(dr[, 3:ncol(dr)], 2, total[3:ncol(dr)], FUN = "/")
  return(dr)
}

# take share and dr table, return A, input-output by industry
share_dr_to_A <- function(share, dr){
  labels <- share[,1:2]
  A_prime <- as.matrix(share[,3:ncol(share)]) %*% as.matrix(dr[,3:ncol(dr)])
  A <- t(A_prime)
  colnames(A) <- t(labels[,1])
  A <- cbind(labels, A)
  return(A)
}

wrapper <- function(year) {
  use <- load_raw(paste0("data/raw/IOTables_1947-1996/IOUse_Before_Redefinitions_PRO_1963-1996_Summary.xlsx"), year)
  make <- load_raw(paste0("data/raw/IOTables_1947-1996/IOMake_Before_Redefinitions_1963-1996_Summary.xlsx"), year)
  share <- make_to_share(make)
  dr <- use_to_dr(use)
  A <- share_dr_to_A(share, dr)
  total_industry_output <- make[, ncol(make)]
  A <- cbind(A, total_industry_output[1:61,])
  return(A)
}

# create vector of years, 1963-1996, as strings
years <- as.character(1963:1996)

# create list of A matrices
A_byyear_1963_1996 <- lapply(years, wrapper)

# label each A matrix with year
names(A_byyear_1963_1996) <- years


# generate A as share of int. input spending ------------------------------------------

# gen_A_int function
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

A_Int_byyear_1963_1996 = gen_A_int(A_byyear_1963_1996)

# save list of A matrices
save(A_byyear_1963_1996, file = "data/cleaned/A_byyear_1963-1996.RData")
save(A_Int_byyear_1963_1996, file = "data/cleaned/A_Int_byyear_1963_1996.RData")