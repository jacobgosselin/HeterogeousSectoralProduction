# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to overall folder
setwd("../")

# clean raw IO data ----------------------------------------------------------

clean_raw = 1 # set to 1 to clean raw IO data
if(clean_raw == 1) {
  source("code/clean_IO/clean_IO_1947-1963.R")
  setwd("../") # reset working directory
  source("code/clean_IO/clean_IO_1963-1996.R")
  setwd("../") # reset working directory
  source("code/clean_IO/clean_IO_1997-2022.R")
  setwd("../") # reset working directory
  source("code/clean_IO/clean_IO_detailed.R")
  setwd("../") # reset working directory
}



# clean crosswalk ---------------------------------------------------------

# load code crosswalk
code_crosswalk <- read_excel("../data/raw/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx", 
                             sheet = "NAICS Codes", skip = 4)

# keep if not missing Summary and missing Sector
code_crosswalk <- subset(code_crosswalk, !is.na(code_crosswalk$Summary) & is.na(code_crosswalk$Sector))

code_crosswalk <- code_crosswalk[, c("Summary", "U.Summary")]
colnames(code_crosswalk) <- c("Code", "Industry Description")

save(code_crosswalk, file = "../data/cleaned/code_crosswalk.RData")

# make L matrices ---------------------------------------------------------

# clear environment and reload data
rm(list=ls())
load("data/cleaned/A_byyear_1947-1962.Rdata")
load("data/cleaned/A_byyear_1963-1996.Rdata")
load("data/cleaned/A_byyear_1997-2022.Rdata")
load("data/cleaned/A_byyear_det.Rdata")

leontief_inv <- function(A) {
  
  io_matrix <- as.matrix(A[,3:(ncol(A)-1)])
  labels <- A[, 1:2]
  total_output <- A$T008
  n <- nrow(io_matrix)
  I <- diag(n)
  L <- solve(I - io_matrix)
  leontief <- cbind(labels, L, total_output)
  colnames(leontief) <- colnames(A[, 1:ncol(A)])
  return(leontief)

}

# generate L_byyear lists
L_byyear_1947_1962 <- lapply(A_byyear_1947_1962, leontief_inv)
L_byyear_1963_1996 <- lapply(A_byyear_1963_1996, leontief_inv)
L_byyear_1997_2022 <- lapply(A_byyear_1997_2022, leontief_inv)
L_byyear_det <- lapply(A_byyear_det, leontief_inv)

save(L_byyear_1947_1962, file = "data/cleaned/L_byyear_1947-1962.Rdata")
save(L_byyear_1963_1996, file = "data/cleaned/L_byyear_1963-1996.Rdata")
save(L_byyear_1997_2022, file = "data/cleaned/L_byyear_1997-2022.Rdata")
save(L_byyear_det, file = "data/cleaned/L_byyear_det.Rdata")

# load and clean BEA KLEMS, GDP, and GO data ------------------------------

source("code/clean_BEA_ILPA&GO.R")
setwd("../") # reset working directory



