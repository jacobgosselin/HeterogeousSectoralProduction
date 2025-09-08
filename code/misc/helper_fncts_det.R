# define function taking pathname, sheetname, and returning make or use table
load_raw <- function(path, sheet){
  
  # load data and subset to relevant portion of sheet
  raw <- read_excel(path, sheet = sheet)
  column_names <- raw[5,]
  row_names <- raw[,1]
  raw <- raw[-c(1:5),]
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


# take make table, return share table (share of commodities produced by industry)
make_to_share <- function(make){
  
  # Combine rows where Code column is 331313 and 331313, as noted in sheet
  # Identify rows for Code 331313 and 331314
  row_331313 <- which(make$Code == 331313)
  row_331314 <- which(make$Code == 331314)
  
  # Sum columns 3 onwards for these rows and keep columns 1 and 2 from 331313
  combined_row <- make[row_331313, ]
  combined_row[, 3:ncol(make)] <- make[row_331313, 3:ncol(make)] + make[row_331314, 3:ncol(make)]
  
  # Calculate position for the combined row
  insert_position <- min(row_331313, row_331314)
  
  # Insert the combined row at the calculated position
  make <- rbind(make[1:(insert_position-1), ], combined_row, make[(insert_position):nrow(make), ])
  
  # Adjust row numbers for removal after insertion
  row_331313 <- row_331313 + 1
  row_331314 <- row_331314 + 1
  
  # Remove original rows for 331313 and 331314
  make <- make[-c(row_331313, row_331314), ]
  
  # go about share as usual
  total <- as.numeric(make[make$`Industry Description` == "Total Commodity Output",])
  make <- make[1:390,1:392] # exclude government portion and totals
  total <- total[1:392] # exclude government portion and totals
  sd <- make
  sd[, 3:ncol(sd)] <- sweep(sd[, 3:ncol(sd)], 2, total[3:ncol(sd)] , FUN = "/")
  return(sd)
}

# take use table, subset
# (used to involve normalizing, but don't do it anymore)
use_to_use <- function(use){
  # total <- as.numeric(use[use$`Commodity Description` == "Total Industry Output",])
  use <- use[1:390, 1:392] # exclude government portion and totals
  return(use)
}

# use: u_ci, commodity X industry, use of commodity c by industry i in $
# share: s_ic, industry X commodity, share of commodity c made by industry i
# A: A_ij = sum_c u_ci * s_ic = use*share (normalized by row totals)
# take use and share table, return A 
use_share_to_A <- function(use,share){
  labels <- share[,1:2]
  A <- as.matrix(use[,3:ncol(use)]) %*% as.matrix(share[,3:ncol(share)])
  
  rowtotal <- rowSums(A)
  rowtotal[rowtotal == 0] <- 1  # check if rowtotal is 0, if so, just keep 0s
  A <- sweep(A, 1, rowtotal, FUN = "/") # divide by row totals
  
  colnames(A) <- t(labels[,1])
  A <- cbind(labels, A)
  colnames(A)[1] <- "Code"
  colnames(A)[2] <- "Industry Description"
  return(A)
}

wrapper <- function(year) {
  use <- load_raw(paste0("data/raw/BEA_Detailed/IOUse_After_Redefinitions_PRO_2017_Detail.xlsx"), year)
  make <- load_raw(paste0("data/raw/BEA_Detailed/IOMake_After_Redefinitions_2017_Detail.xlsx"), year)
  share <- make_to_share(make)
  use <- use_to_use(use)
  A <- use_share_to_A(use, share)
  total_industry_output <- make[, ncol(make)]
  colnames(total_industry_output) <- c("T008")
  A <- cbind(A, total_industry_output[1:390,])
  return(A)
}