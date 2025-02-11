# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to overall folder
setwd("../")

# pull API data ----------------------------------------------------------

pull_api = 0 # set to 1 to clean raw IO data
if(pull_api == 1) {
  source("code/cleaning/api_pull_BEA.R")
  # source("code/cleaning/api_pull_Compustat.R") (can't include in public repo)
}

# clean raw data ---------------------------------------------------------

# generate BEA Code-Industry Description crosswalk
library(readxl)
code_desc_crosswalk <- read_excel("data/raw/naics_crosswalk/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx", 
                             sheet = "NAICS Codes", skip = 4)
code_desc_crosswalk <- subset(code_desc_crosswalk, !is.na(code_desc_crosswalk$Summary) & is.na(code_desc_crosswalk$Sector))
code_desc_crosswalk <- code_desc_crosswalk[, c("Summary", "U.Summary")]
colnames(code_desc_crosswalk) <- c("Code", "Industry Description")
save(code_desc_crosswalk, file = "data/cleaned/code_desc_crosswalk.RData")

# generate BEA NAICS crosswalk
source("code/cleaning/clean_BEA_NAICS_crosswalk.R")

# clean BEA data
source("code/cleaning/clean_IO_1997-2023.R") # IO data
source("code/cleaning/clean_BEA_prices.R") # clean price data
source("code/cleaning/clean_BEA_ILPA&GO.R") # clean additional BEA data
# clean patent data
# source("code/cleaning/clean_patents.R") (can't include in public repo)

# make resid data ---------------------------------------------------------

source("code/building/build_residdata_1997-2023.R")

# make atalay instrument data ---------------------------------------------

source("code/building/build_atalay_instruments.R")

# make BEA calibration data -----------------------------------------------

source("code/building/build_BEA_calibration.R")

