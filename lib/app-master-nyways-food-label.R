# app master file for NYWAYS food Label



source("lib/usda-wrappers.R")
source("lib/nyways-food-label-interpreter.R")
source("lib/nyways-database-apis.R")
source("secure/creds.R")

# FUNCTION WRAPPERS - using USDA formats ####

app.calculate.allScores <- function(foodDetails.list){
  parsed.data <- usda.details.parseCommonNutrients(foodDetails.list)
  
  tempResults.df <- nyways.macros.allScores(
    name = parsed.data$name,
    g_fat = parsed.data$fat,
    g_carbs = parsed.data$carbs,
    g_protein = parsed.data$protein,
    g_sugar = parsed.data$sugar,
    g_sugarAlcohol = 0,
    g_satFat = parsed.data$satFat,
    g_fiber = parsed.data$fiber,
    mg_sodium = parsed.data$sodium,
    kcals = parsed.data$kcals,
    g_serving = parsed.data$servingSize,
    pctVeggies = 0
  )
  
  return(tempResults.df)
}

app.giveScoresFromUPC <- function(UPC){
  details.df <- usda.searchByUPC(UPC, creds.usda)
  results.df <- app.calculate.allScores(details.df)
  return(results.df)
}

app.get
