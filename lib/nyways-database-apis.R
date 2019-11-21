# nyways database access
# this is giving space for future scope - as of now this is not striclty required and uses offline data


require(httr)
require(jsonlite)
# Settings
# nyways.api.path <- 
foodDataFolder <- "db/foods"

# general helper functions ####
loadDataFromCSV <- function(folderPath){
  filenames <- list.files(folderPath, pattern="*.csv", full.names=TRUE)
  
  ldf <- lapply(filenames, read.csv)

  return(ldf)
}

naCheck <- function(valueToCheck, replaceBy = ""){
  return(
    ifelse(is.na(valueToCheck),replaceBy, valueToCheck)
  )
}


pasteColumns <- function(array1, array2, sep = ", "){
  tempResult <- paste((array1), (array2), sep = sep)
  
  return(tempResult)
}

# specific helper functions ####

pasteFoodData <- function(food.df, newname, col1, col2, sep = ", ", deleteOriginals = T){
  
  tempA <- as.character(food.df[,col1])
  tempB <- as.character(food.df[,col2])
  
  if(deleteOriginals){
    food.df[col1] <- NULL
    food.df[col2] <- NULL
  }
  
  # food.df[newname] <- pasteColumns(tempA, tempB, sep = sep)
  food.df[newname] <- apply(cbind(tempA, tempB), 1, function(x) paste(x[!is.na(x)], collapse = sep))
  
  return(food.df)
  
}

mergeFoodData <- function(foods.list){

  ldf <- foods.list
  
  mergedFoods.df <- ldf[[1]]

  l <- length(ldf)
  
  if (l > 1 ) {
    for (i in 2:length(ldf)){
      mergedFoods.df <- merge(mergedFoods.df, ldf[[i]], by = "Name", all = TRUE)

      mergedFoods.df$NutriRating <- naCheck(mergedFoods.df$NutriRating.x, replaceBy = 0) + naCheck(mergedFoods.df$NutriRating.y, replaceBy = 0)

      mergedFoods.df$NutriRating.x <- NULL
      mergedFoods.df$NutriRating.y <- NULL

      mergedFoods.df <- pasteFoodData(mergedFoods.df, 'Description', 'Description.y', 'Description.x')
      mergedFoods.df <- pasteFoodData(mergedFoods.df, 'Purpose', 'Purpose.y', 'Purpose.x')

    }
  }
  
  return(mergedFoods.df)
}

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

nyways.api.getIngredients <- function(local=F){
  # include here lists of positive and negative ingredients
  # we are working from a general database table of ingredients
  # this table is then split according to parameters for data analysis

  if(local==F){
    return(NULL)
  } else {
    # load the data and merge into single data frame
    tempFoods.list <- loadDataFromCSV(foodDataFolder)
    foods.df <- mergeFoodData(tempFoods.list)
    
    # combine a little more for ease of use
    foods.df <- pasteFoodData(foods.df, 'Description', 'Description', 'Code', deleteOriginals = F, sep = " - ")

    foods.df$Name <- toupper(foods.df$Name)
    
  }
  
  return(foods.df)
}