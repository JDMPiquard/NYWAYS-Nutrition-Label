# USDA Food Wrapper ;)
# 2019 Nov 9


require(httr)

# Settings ####
usda.endpoint <- "https://api.nal.usda.gov/fdc/v1/"
# note that an API Key needs to be supplied with each call

# Wrappers ####
check200 <- function(httpResponse){
  if(httpResponse$status_code == 200){
    return(T)
  } else {
    return(F)
  }
}

# FOOD SEARCH - working
usda.search <- function(searchString, # note that more fields are available as per https://fdc.nal.usda.gov/api-guide.html
                        api_key,
                        Branded = T, 
                        Foundation = T,
                        Survey = T
){
  usda.search.endpoint <- paste0(usda.endpoint,
                                 "search",
                                 "?api_key=",
                                 api_key)
  
  usda.search.body <- list(
    generalSearchInput = searchString,
    includeDataTypes = list(
      "Survey (FNDDS)" = Survey,
      Foundation = Foundation,
      Branded = Branded
    )
  )
  
  postResponse <- POST(
    usda.search.endpoint,
    body = usda.search.body,
    encode = "json"
  )
  
  if(check200(postResponse)){
    tempContent <- content(postResponse)
  } else {
    tempContent <- NULL
  }
  
  return(tempContent)
}

# FOOD DETAILS
usda.details <- function(fdcId,
                         api_key){
  usda.details.endpoint <- paste0(usda.endpoint,
                                  fdcId,
                                 "?api_key=",
                                 api_key)
  
  getResponse <- GET(usda.details.endpoint)
  
  if(check200(getResponse)){
    tempContent <- content(getResponse)
  } else {
    tempContent <- NULL
  }
  
  return(tempContent)
}

# Generic Helpers ####
simple_list_to_df <- function(simpleList){
  l <- length(simpleList)
  if(l == 0){
    return(NULL)
  }
  
  temp.list <- lapply(simpleList, as.data.frame)
  
  result.df <- temp.list[[1]]
  
  if(l == 1){
    
  } else {
    for(i in 2:l){
      result.df <- rbind(result.df,temp.list[[i]])
    }
  }
  
  return(result.df)
}

convertNULLtoBlank <- function(valueToCheck){
  return(ifelse(is.null(valueToCheck),"",valueToCheck))
}

# Parsers ####

# Helper: only extract the useful and consistent data
usda.search.parsed.standardise <- function(searchListItem){
  tempResult.list <- list(
    fdcId = searchListItem$fdcId,
    description = searchListItem$description,
    dataType = searchListItem$dataType,
    brandOwner = searchListItem$brandOwner,
    gtinUpc = searchListItem$gtinUpc
  )
  
  tempResult.list <- lapply(tempResult.list, convertNULLtoBlank)
  
  return(tempResult.list)
}

# SEARCH
usda.search.parsed <- function(searchString, # note that more fields are available as per https://fdc.nal.usda.gov/api-guide.html
                               api_key,
                               Branded = T, 
                               Foundation = T,
                               Survey = T){
  
  temp.results.list <- usda.search(searchString,
                                   api_key,
                                   Branded, 
                                   Foundation,
                                   Survey)
  
  temp.results.list.clean <- lapply(temp.results.list$foods, usda.search.parsed.standardise)
  
  results.df <- simple_list_to_df(temp.results.list.clean)
  
  return(results.df)
  
}

# DETAILS Helper - only extract consistent data
usda.details.parsed.standardise <- function(detailsListItem){
  tempResult.list <- list(
    id = detailsListItem$nutrient$id,
    number = detailsListItem$nutrient$number,
    name = detailsListItem$nutrient$name,
    unitName = detailsListItem$nutrient$unitName,
    amount = detailsListItem$amount
  )
  
  tempResult.list <- lapply(tempResult.list, convertNULLtoBlank)
  
  return(tempResult.list)
}

# DETAILS
usda.details.parsed <- function(fdcId,
                                api_key){
  
  temp.results.list <- usda.details(fdcId, api_key)
  
  if(is.null(temp.results.list)){
    return(NULL)
  }
  
  tempNutrients.list <- lapply(temp.results.list$foodNutrients, usda.details.parsed.standardise)
  
  tempNutrients.df <- simple_list_to_df(tempNutrients.list)
  
  servingSize <- temp.results.list$servingSize
  
  tempNutrients.df$nutrient.perServing <- tempNutrients.df$amount*servingSize/100
  
  results.list <- temp.results.list
  results.list$foodNutrients <- NULL
  results.list$foodAttributes <- NULL
  results.list$foodComponents <- NULL
  results.list$foodPortions <- NULL
  
  results.list$nutrients = tempNutrients.df
  results.list$servingSize = servingSize
  results.list$ingredients = trimws(unlist(strsplit(results.list$ingredients, ",")))
  
  return(results.list)
}

# GET SPECIFIC NUTRIENT
usda.details.getNutrientByID <- function(foodDetails.list, nutrientID){
  
  tempResult <- foodDetails.list$nutrients[foodDetails.list$nutrients$id==nutrientID,]$amount
  
  result <- ifelse(length(tempResult) == 0, 0, tempResult)
  
  return(result)
}

usda.details.parseCommonNutrients <- function(foodDetails.list){
  
  productName <- paste(foodDetails.list$brandOwner, foodDetails.list$description)
  
  kcals <- usda.details.getNutrientByID(foodDetails.list, 1008)
  sugar <- usda.details.getNutrientByID(foodDetails.list, 2000)
  satFat <- usda.details.getNutrientByID(foodDetails.list, 1258) 
  sodium <- usda.details.getNutrientByID(foodDetails.list, 1093) 
  # sadly there is no reliable way to get % vegetables and fruits, so set to 0 for now
  pctVeggies <- 0
  # get the positive scores by ID
  fiber <- usda.details.getNutrientByID(foodDetails.list, 1079) 
  protein <- usda.details.getNutrientByID(foodDetails.list, 1003)
  
  # get other common stuff
  fat <- usda.details.getNutrientByID(foodDetails.list, 1004) 
  carbs <- usda.details.getNutrientByID(foodDetails.list, 1005) 
  
  # can also get trans, vitamins and added sugars, but need to handle the case where it does not exist
  sugarsAdded <- usda.details.getNutrientByID(foodDetails.list, 1235)
  fatTrans <- usda.details.getNutrientByID(foodDetails.list, 1257)
  
  results <- list(
    name = productName,
    servingSize = foodDetails.list$servingSize,
    kcals = kcals,
    sugar = sugar,
    satFat = satFat,
    sodium = sodium,
    pctVeggies = pctVeggies,
    fiber = fiber,
    protein = protein,
    fat = fat,
    carbs = carbs,
    sugarsAdded = sugarsAdded,
    fatTrans = fatTrans
  )
  
  return(results)
}

# HELPFUL WRAPPERS ####
usda.searchByUPC <- function(upc, # note that more fields are available as per https://fdc.nal.usda.gov/api-guide.html
                             api_key){
  
  upc <- as.character(upc)
  
  temp.search <- usda.search.parsed(upc, # note that more fields are available as per https://fdc.nal.usda.gov/api-guide.html
                                    api_key,
                                    Branded = T, 
                                    Foundation = F,
                                    Survey = F)
  
  if(is.null(temp.search)){
    return(NULL)
  }
  
  temp.fdcId <- temp.search$fdcId[1] # always gets the first result
  
  
  results <- usda.details.parsed(temp.fdcId, api_key)
  
  return(results)
  
}

