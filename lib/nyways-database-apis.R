# nyways database access
# this is giving space for future scope - as of now this is not striclty required and uses offline data


require(httr)
require(jsonlite)
# Settings
# nyways.api.path <- 

# get common use data ####
nyways.api.getMacros <- function(local=F){
  macros.json <- ""
  
  if(local==F){
    return(NULL) # will not be available until API or similar is available
  } else {
    # if using local data, this is just provided as an example
    macros.json <- '{
  "keto": {
    "proteinMin": 0.15,
    "proteinMax": 0.2,
    "fatMin": 0.75,
    "fatMax": 0.8,
    "carbsMin": 0.1,
    "carbsMax": 0.05
  },
    "zone": {
    "proteinMin": 0.275,
    "proteinMax": 0.325,
    "fatMin": 0.275,
    "fatMax": 0.325,
    "carbsMin": 0.375,
    "carbsMax": 0.425
    }
}'
  }
  
  macros.list <- fromJSON(macros.json)
  
  return(macros.list)
}