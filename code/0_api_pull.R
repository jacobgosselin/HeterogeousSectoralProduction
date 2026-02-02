library(httr)
library(jsonlite)
setwd('/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/het_sectoral_prod/V2')

# pulling data from api
creds <- fromJSON('/Users/jacobgosselin/Library/CloudStorage/GoogleDrive-jacob.gosselin@u.northwestern.edu/My Drive/research_ideas/misc/api_keys.txt')
beaKey = creds$BEA # have to put in your own
# parse json data from url
getDatIO <- function(tableID) {
  url = paste0("https://apps.bea.gov/api/data/?&UserID=", beaKey, "&method=GetData&DataSetName=InputOutput&Year=ALL&tableID=", tableID, "&ResultFormat=json")
  response <- GET(url)
  data <- content(response, "text")
  json_data <- fromJSON(data)
  return(json_data)
}
getDatGDP <- function(tableID) {
  url = paste0("https://apps.bea.gov/api/data/?&UserID=", beaKey, "&method=GetData&DataSetName=GDPbyIndustry&Year=ALL&tableID=", tableID, "&Industry=ALL&Frequency=A&ResultFormat=json")
  response <- GET(url)
  data <- content(response, "text")
  json_data <- fromJSON(data)
  return(json_data)
}
getDatNIPA <- function(tableName) {
  url = paste0("https://apps.bea.gov/api/data/?&UserID=", beaKey, "&method=GetData&DataSetName=NIPA&Year=ALL&TableName=", tableName, "&Frequency=A&ResultFormat=json")
  response <- GET(url)
  data <- content(response, "text")
  json_data <- fromJSON(data)
  return(json_data)
}

# list of table IDs to confirm I'm getting correct data
IO_Table_List = GET("https://apps.bea.gov/api/data?&UserID=2796DD07-FDBD-4424-83E9-5D8AD49673D6&method=GetParameterValues&datasetname=InputOutput&ParameterName=TableID&ResultFormat=json")
IO_Table_List <- content(IO_Table_List, "text")
IO_Table_List <- fromJSON(IO_Table_List)$BEAAPI$Results$ParamValue
GDP_Table_List = GET("https://apps.bea.gov/api/data?&UserID=2796DD07-FDBD-4424-83E9-5D8AD49673D6&method=GetParameterValues&datasetname=GDPbyIndustry&ParameterName=TableID&ResultFormat=json")
GDP_Table_List <- content(GDP_Table_List, "text")
GDP_Table_List <- fromJSON(GDP_Table_List)$BEAAPI$Results$ParamValue

# get supply and Use data for all years
supply_JSON_raw <- getDatIO("262")
supply_data_raw <- supply_JSON_raw$BEAAPI$Results$Data[[1]]
use_JSON_raw <- getDatIO("259")
use_data_raw <- use_JSON_raw$BEAAPI$Results$Data[[1]]
save(supply_JSON_raw, supply_data_raw, use_JSON_raw, use_data_raw, file = "raw_data/BEA_supply&use.RData")

# get Price data for all years
output_price_JSON_raw <- getDatGDP("18")
output_price_data_raw <- output_price_JSON_raw$BEAAPI$Results$Data[[1]]
II_price_JSON_raw <- getDatGDP("23")
II_price_data_raw <- II_price_JSON_raw$BEAAPI$Results$Data[[1]]
save(output_price_JSON_raw, output_price_data_raw, II_price_JSON_raw, II_price_data_raw, file = "raw_data/BEA_price.RData")

# get Defense Spending
defense_use_JSON_raw <- getDatNIPA("T31106")
defense_spending <- defense_use_JSON_raw$BEAAPI$Results$Data[[1]]
save(defense_use_JSON_raw, defense_spending, file = "raw_data/BEA_defense_spending.RData")