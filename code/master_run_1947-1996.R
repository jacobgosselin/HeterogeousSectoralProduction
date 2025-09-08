# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to overall folder
setwd("../")

# libraries
library(readxl)

# pull API data ----------------------------------------------------------

pull_api = 0 # set to 1 to clean raw IO data
if(pull_api == 1) {
  source("code/cleaning/api_pull_BEA.R")
  source("code/cleaning/api_pull_Compustat.R")
}

# clean raw data ---------------------------------------------------------

# generate BEA Code-Industry Description crosswalk
code_desc_crosswalk <- read_excel("data/raw/naics_crosswalk/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx", 
                             sheet = "NAICS Codes", skip = 4)
code_desc_crosswalk <- subset(code_desc_crosswalk, !is.na(code_desc_crosswalk$Summary) & is.na(code_desc_crosswalk$Sector))
code_desc_crosswalk <- code_desc_crosswalk[, c("Summary", "U.Summary")]
colnames(code_desc_crosswalk) <- c("Code", "Industry Description")
save(code_desc_crosswalk, file = "data/cleaned/code_desc_crosswalk.RData")

# generate BEA NAICS crosswalk
source("code/cleaning/clean_BEA_NAICS_crosswalk.R")

# clean BEA data
source("code/cleaning/clean_IO_1947-1963.R") # IO 1947-1963
source("code/cleaning/clean_IO_1963-1996.R") # IO 1963-1996
source("code/cleaning/clean_BEA_prices_1947-1996.R") # clean BEA price data

# make resid data ---------------------------------------------------------

source("code/building/build_residdata_1947-1996.R")