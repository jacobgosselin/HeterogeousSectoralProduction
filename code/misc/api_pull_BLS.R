library(jsonlite)
library(blsAPI)

payload <- list(
  'seriesid'  = c('EIUIR14', 'EIUIR15'),
  'startyear' = 2007,
  'endyear'   = 2009,
  "registrationkey" = "dba2695237064d33a578704eb1187e9a")
response <- blsAPI(payload, 2)
json     <- fromJSON(response)

## Process results
apiDF <- function(data) {
  df  <- data.frame(data)
  return(df)
}

test.df  <- apiDF(json$Results$series$data[[1]])

# get first 3 digits of NAICS
BEA_NAICS_crosswalk$three_dig <- substr(BEA_NAICS_crosswalk$NAICS, 1, 3)
# eliminate duplicates of Summary/three_dig
unique_summary_threedig_combo <- unique(BEA_NAICS_crosswalk[,c("Summary", "three_dig")])
