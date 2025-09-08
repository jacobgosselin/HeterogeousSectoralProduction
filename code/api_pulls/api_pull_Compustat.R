# set current directory to code location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# move to data subfolder
setwd("../../data")

# load libraries
library(RPostgres)
library(data.table)

# loading CRSP data
# using CRSP/Compustat Merged Database-Linking Table
# https://wrds-www.wharton.upenn.edu/pages/get-data/center-research-security-prices-crsp/annual-update/crspcompustat-merged/compustat-crsp-link/

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='jacobgosselin')

res <- dbSendQuery(wrds, " select crsp.ccmxpf_lnkhist.gvkey, crsp.ccmxpf_lnkhist.lpermno as permno, crsp.ccmxpf_lnkhist.linkenddt as last_link_date, comp.company.naics
                    from crsp.ccmxpf_lnkhist
                    JOIN comp.company -- Reference to the company table
                    ON crsp.ccmxpf_lnkhist.gvkey = comp.company.gvkey -- Joining on gvkey")
crsp_data <- data.table(dbFetch(res))
dbClearResult(res)
dbDisconnect(wrds)

# subset to unique permno
crsp_data <- subset(crsp_data, !is.na(permno) & !is.na(naics)) # can only use if not missing permno and naics
crsp_data <- crsp_data[, c("permno", "naics")]
crsp_data <- subset(crsp_data, !duplicated(permno))
save(crsp_data, file = "../data/raw/api_pull_Compustat/crsp_data.RData")