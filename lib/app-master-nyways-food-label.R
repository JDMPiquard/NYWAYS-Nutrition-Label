# app master file for NYWAYS food Label



source("lib/usda-wrappers.R")
source("lib/nyways-food-label-interpreter.R")
source("lib/nyways-database-apis.R")
source("secure/creds.R")

# FUNCTION WRAPPERS - using USDA formats ####
app.calculate.nutriscore <- function(foodDetails.list){
  # get the negative scores by ID
  kcals <- usda.details.getNutrientByID(foodDetails.list, 1008)
  sugar <- usda.details.getNutrientByID(foodDetails.list, 2000)
  satFat <- usda.details.getNutrientByID(foodDetails.list, 1258) 
  sodium <- usda.details.getNutrientByID(foodDetails.list, 1093) 
  # sadly there is no reliable way to get % vegetables and fruits, so set to 0 for now
  pctVeggies <- 0
  # get the positive scores by ID
  fiber <- usda.details.getNutrientByID(foodDetails.list, 1079) 
  protein <- usda.details.getNutrientByID(foodDetails.list, 1003)
  
  results <- nutriscore.beta(
    kcals = kcals,
    sugar = sugar,
    satFat = satFat,
    sodium = sodium,
    pctVeggies = pctVeggies,
    fiber = fiber,
    protein = protein
  )
  
  return(results)
}

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
