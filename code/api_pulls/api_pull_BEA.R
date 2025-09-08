library(httr)
library(jsonlite)

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

# list of table IDs to confirm I'm getting supply and Use for all years
IO_Table_List = GET("https://apps.bea.gov/api/data?&UserID=2796DD07-FDBD-4424-83E9-5D8AD49673D6&method=GetParameterValues&datasetname=InputOutput&ParameterName=TableID&ResultFormat=json")
IO_Table_List <- content(IO_Table_List, "text")
IO_Table_List <- fromJSON(IO_Table_List)$BEAAPI$Results$ParamValue
GDP_Table_List = GET("https://apps.bea.gov/api/data?&UserID=2796DD07-FDBD-4424-83E9-5D8AD49673D6&method=GetParameterValues&datasetname=GDPbyIndustry&ParameterName=TableID&ResultFormat=json")
GDP_Table_List <- content(GDP_Table_List, "text")
GDP_Table_List <- fromJSON(GDP_Table_List)$BEAAPI$Results$ParamValue
NIPA_Table_List = GET("https://apps.bea.gov/api/data?&UserID=2796DD07-FDBD-4424-83E9-5D8AD49673D6&method=GetParameterValues&datasetname=NIPA&ParameterName=TableName&ResultFormat=json")
NIPA_Table_List <- content(NIPA_Table_List, "text")
NIPA_Table_List <- fromJSON(NIPA_Table_List)$BEAAPI$Results$ParamValue

# get Defense Spending
defense_use_JSON_raw <- getDatNIPA("T31106")
defense_spending <- defense_use_JSON_raw$BEAAPI$Results$Data[[1]]

# get supply and Use data for all years
supply_JSON_raw <- getDatIO("262")
supply_data_raw <- supply_JSON_raw$BEAAPI$Results$Data[[1]]
use_JSON_raw <- getDatIO("259")
use_data_raw <- use_JSON_raw$BEAAPI$Results$Data[[1]]
save(supply_JSON_raw, supply_data_raw, use_JSON_raw, use_data_raw, file = "data/raw/api_pull_BEA/supply&use.RData")

# get Price data for all years
output_price_JSON_raw <- getDatGDP("18")
output_price_data_raw <- output_price_JSON_raw$BEAAPI$Results$Data[[1]]
II_price_JSON_raw <- getDatGDP("23")
II_price_data_raw <- II_price_JSON_raw$BEAAPI$Results$Data[[1]]
save(output_price_JSON_raw, output_price_data_raw, II_price_JSON_raw, II_price_data_raw, file = "data/raw/api_pull_BEA/price.RData")

# get gross output/value-added data for all years
gross_output_JSON_raw <- getDatGDP("15")
gross_output_data_raw <- gross_output_JSON_raw$BEAAPI$Results$Data[[1]]
value_added_JSON_raw <- getDatGDP("10")
value_added_raw <- value_added_JSON_raw$BEAAPI$Results$Data[[1]]
real_gross_output_JSON_raw <- getDatGDP("208")
real_gross_output_data_raw <- real_gross_output_JSON_raw$BEAAPI$Results$Data[[1]]
save(gross_output_JSON_raw, gross_output_data_raw, value_added_JSON_raw, value_added_raw, real_gross_output_JSON_raw, real_gross_output_data_raw, file = "data/raw/api_pull_BEA/GrossOutput_ValueAdded.RData")

# get GDP for all years
gdp_JSON_raw <- getDatNIPA("T10101")
gdp_data_raw <- gdp_JSON_raw$BEAAPI$Results$Data
save(gdp_JSON_raw, gdp_data_raw, file = "data/raw/api_pull_BEA/GDP.RData")
