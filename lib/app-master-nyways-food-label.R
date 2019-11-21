# app master file for NYWAYS food Label



source("lib/usda-wrappers.R")
source("lib/nyways-food-label-interpreter.R")
source("lib/nyways-database-apis.R")
source("secure/creds.R")

# Load necessary data ###
nyways.foods.df <- nyways.api.getIngredients(local = T)

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

# create compound ingredient list
app.parse.ingredients <- function(foodDetails.list){
  temp.pattern <- toupper(paste(foodDetails.list$ingredients, collapse = "|"))
  
  # need to understand how to configure agrepl correctly (Levenshtein distance, etc)
  temp.matches <- grepl(temp.pattern, nyways.foods.df$Name, ignore.case = T, fixed = F)
  temp.matches.df <- nyways.foods.df[temp.matches,]
  
  results.df <- data.frame(Name = foodDetails.list$ingredients, R = 1:length(foodDetails.list$ingredients))
  
  results.df <- merge(results.df, temp.matches.df, all.x = T)
  
  return(results.df)
}

# helper for prettification
emojiRating <- function(valueToCheck){
  valueToCheck = naCheck(valueToCheck, replaceBy = 0)
  
  if(valueToCheck > 0){
    return("✅")
  } else if(valueToCheck < 0 ){
    return("🛑")
  } else {
    return("▫️")
  }
}

app.prettify.ingredients <- function(foodDetails.list){
  temp.df <- foodDetails.list$parsedIngredients
  
  tempRating <- unlist(lapply(temp.df$NutriRating, emojiRating)
)  
  results.df <- data.frame(
    R = temp.df$R,
    Score = tempRating,
    Name = temp.df$Name,
    Purpose = temp.df$Purpose,
    Description = temp.df$Description
  )
  
  results.df <- results.df[order(results.df$R),]
  
  results.df$R <- NULL
  rownames(results.df) <- NULL
  
  return(results.df)
}

# One ####
app.getFullDetailsFromUPC <- function(UPC){
  results.df <- usda.searchByUPC(UPC, creds.usda)
  results.df$allScores <- app.calculate.allScores(results.df)
  results.df$parsedIngredients <- app.parse.ingredients(results.df)
  return(results.df)
}