# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to data subfolder
setwd("../../data")
# load libraries
library(readxl)


# load and clean int. input data ------------------------------------------

intermediate_inputs_1947_1997 <- read_excel("raw/AllTablesHist/GDPbyInd_II_1947-1997.xlsx", 
                                  sheet = "II", skip = 5)
intermediate_inputs_1947_1997$'Industry Description' <- intermediate_inputs_1947_1997$...2
intermediate_inputs_1947_1997 <- intermediate_inputs_1947_1997[, -c(1:2)]

# generate A as share of int. input spending ------------------------------

# gen_A_tilde function
gen_A_tilde <- function(A_byyear, II) {
  industryuse_byyear <- A_byyear
  for (i in 1:length(A_byyear)) {
    industryuse_byyear[[i]][,3:ncol(A_byyear[[i]])] <- A_byyear[[i]][,3:ncol(A_byyear[[i]])] * A_byyear[[i]][,ncol(A_byyear[[i]])]
  }
  
  # merge each item with respective year column in intermediate inputs
  for (i in names(industryuse_byyear)) {
    year = as.character(i)
    temp_intermediate <- II[, c(year, "Industry Description")]
    colnames(temp_intermediate) <- c("Total Intermediate", "Industry Description")
    temp_intermediate$'Total Intermediate' <- as.numeric(temp_intermediate$'Total Intermediate')
    industryuse_byyear[[year]] <- merge(industryuse_byyear[[year]], temp_intermediate, by = "Industry Description")
    # divide columns 3:ncol-1 by Total Intermediate
    industryuse_byyear[[year]][,3:(ncol(industryuse_byyear[[year]])-1)] <- industryuse_byyear[[year]][,3:(ncol(industryuse_byyear[[year]])-1)] / industryuse_byyear[[year]]$'Total Intermediate'
  }
  return(industryuse_byyear)

}

load("cleaned/A_byyear_1947-1962.RData")
load("cleaned/A_byyear_1963-1996.RData")

A_Int_byyear_1947_1962 = gen_A_tilde(A_byyear_1947_1962, intermediate_inputs_1947_1997)
A_Int_byyear_1963_1996 = gen_A_tilde(A_byyear_1963_1996, intermediate_inputs_1947_1997)

save(A_Int_byyear_1947_1962, file = "cleaned/A_Int_byyear_1947_1962.RData")
save(A_Int_byyear_1963_1996, file = "cleaned/A_Int_byyear_1963_1996.RData")

