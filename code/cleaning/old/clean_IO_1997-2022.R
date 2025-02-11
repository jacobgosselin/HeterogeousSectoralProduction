# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to data subfolder
setwd("../../data")

# load libraries
library(readxl)

# define function taking pathname, sheetname, and returning make or use table
load_raw <- function(path, sheet){
  
  # load data and subset to relevant portion of sheet
  raw <- read_excel(path, sheet = sheet)
  column_names <- raw[5,]
  row_names <- raw[,1]
  raw <- raw[-c(1:6),]
  colnames(raw) <- column_names
  # convert columns 3 on to numeric
  raw[,3:ncol(raw)] <- lapply(raw[,3:ncol(raw)], as.numeric)
  # replace NAs with 0s
  raw[,3:ncol(raw)][is.na(raw[,3:ncol(raw)])] <- 0
 
  # if has column `Industries/Commodities`, subset to non-missing rows
  if("Industries/Commodities" %in% colnames(raw)){
    raw <- raw[!is.na(raw$`Industries/Commodities`),]
  }
  # if has column `Commodities/Industries`, subset to non-missing rows
  if("Commodities/Industries" %in% colnames(raw)){
    raw <- raw[!is.na(raw$`Commodities/Industries`),]
  }
  
  return(raw)
}


# take make table, return share table
make_to_share <- function(make){
  total <- as.numeric(make[make$`Industries/Commodities` == "Total Commodity Output",])
  make <- make[1:66,1:68] # exclude government portion and totals
  total <- total[1:68] # exclude government portion and totals
  sd <- make
  sd[, 3:ncol(sd)] <- sweep(sd[, 3:ncol(sd)], 2, total[3:ncol(sd)] , FUN = "/")
  return(sd)
}

# take use table, return directrequirements table
use_to_dr <- function(use){
  total <- as.numeric(use[use$`Commodities/Industries` == "Total Industry Output",])
  use <- use[1:66, 1:68] # exclude government portion and totals
  total <- total[1:68] # exclude government portion and totals
  dr <- use
  dr[, 3:ncol(dr)] <- sweep(dr[, 3:ncol(dr)], 2, total[3:ncol(dr)], FUN = "/")
  return(dr)
}

# take share and dr table, return A, input-output by industry
share_dr_to_A <- function(share, dr){
  labels <- share[,1:2]
  A_prime <- as.matrix(share[,3:ncol(share)]) %*% as.matrix(dr[,3:ncol(dr)]) # DR' X S' = (S X DR)'
  A <- t(A_prime)
  colnames(A) <- t(labels[,1])
  A <- cbind(labels, A)
  colnames(A)[1] <- "Code"
  colnames(A)[2] <- "Industry Description"
  return(A)
}

wrapper <- function(year) {
  use <- load_raw(paste0("./raw/AllTablesIO_1997-2022/IOUse_After_Redefinitions_PRO_1997-2022_Summary.xlsx"), year)
  make <- load_raw(paste0("./raw/AllTablesIO_1997-2022/IOMake_After_Redefinitions_PRO_1997-2022_Summary.xlsx"), year)
  share <- make_to_share(make)
  dr <- use_to_dr(use)
  A <- share_dr_to_A(share, dr)
  total_industry_output <- make[, ncol(make)]
  colnames(total_industry_output) <- c("T008")
  A <- cbind(A, total_industry_output[1:66,])
  return(A)
}

# create vector of years, 1997-2022, as strings
years <- as.character(1997:2022)

# create list of A matrices
A_byyear_1997_2022 <- lapply(years, wrapper)

# label each A matrix with year
names(A_byyear_1997_2022) <- years

# save list of A matrices
save(A_byyear_1997_2022, file = "./cleaned/A_byyear_1997-2022.RData")
rm(list = ls())
