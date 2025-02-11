# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to overall folder
setwd("../")

# clean raw IO data ----------------------------------------------------------

source("code/clean_IO_1997-2022.R")
setwd("../")
source("code/clean_int_IO_1997-2022.R")
setwd("../")

# make digraphs -----------------------------------------------------------

source("code/make_digraphs.R")
for (i in 1:length(A_byyear_1997_2022)) {
  save_plot(A_byyear_1997_2022[[i]], names(A_byyear_1997_2022)[i], "summary", "A")
  save_plot(L_byyear_1997_2022[[i]], names(L_byyear_1997_2022)[i], "summary", "L")
}

# load and clean BEA KLEMS, GDP, and GO data ------------------------------

source("code/clean_BEA_ILPA&GO.R")
setwd("../")

# load and clean patent data ----------------------------------------------

source("code/clean_patents.R")
setwd("../")


