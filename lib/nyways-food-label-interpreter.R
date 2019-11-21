# NYWAYS Food Label interpreter
# requires usda-wrappers.R



# Checking ingredient quality ####


# Creating basic scores ####
displayAsPct <- function(fraction){
  result <- paste0(round(fraction*100), "%")
  
  return(result)
}

nyways.macros.calculate.macroSplit <- function(g_fat, g_carbs, g_protein){
  fat_cals <- 9*g_fat
  protein_cals <- 4*g_protein
  carb_cals <- 4*g_carbs
  calculated_cals <- fat_cals + protein_cals + carb_cals
  
  fat_pct <- fat_cals/calculated_cals
  protein_pct <- protein_cals/calculated_cals
  carbs_pct <- carb_cals/calculated_cals
  
  results.list <-  list(
    fat_pct = fat_pct,
    carbs_pct = carbs_pct,
    protein_pct = protein_pct
    )
  
  return(results.list)
}

nyways.macros.calculate.basicScores <- function(
  g_fat,
  g_carbs,
  g_protein,
  g_sugar,
  g_sugarAlcohol = 0,
  g_fiber,
  kcals,
  g_serving
){
  # net carbs
  netCarbs <- g_carbs - g_fiber - g_sugarAlcohol
  
  # grams protein per 100 kcals
  proteinCalRatio <- round(g_protein/kcals*100)
  proteinCalRatio.score <- ifelse(
    proteinCalRatio >= 20, "Extreme!", ifelse(
      proteinCalRatio >= 15, "Very High", ifelse(
        proteinCalRatio >= 10, "High", ifelse(
          proteinCalRatio >= 6, "Medium-High", ifelse(
            proteinCalRatio >= 3, "Medium-Low", ifelse(
              proteinCalRatio >= 1, "Low", "None"
            )
          ) 
        )
      )
    )
  )
  
  results.list <- list(
    netCarbs = netCarbs,
    proteinRatio = proteinCalRatio,
    proteinScore = proteinCalRatio.score
  )
  
  return(results.list)
}

# Checking macro profiles ####

# this is not really working
nyways.macros.single <- function(macros.list.single, g_fat, g_carbs, g_fiber = 0, g_protein){
  
  fat_cals <- 9*g_fat
  protein_cals <- 4*g_protein
  carb_cals <- 4*g_carbs
  calculated_cals <- fat_cals + protein_cals + carb_cals
  print(calculated_cals)
  
  fat <- fat_cals/calculated_cals
  protein <- protein_cals/calculated_cals
  carbs <- carb_cals/calculated_cals
  
  if(fat < macros.list.single$fatMax
     & fat > macros.list.single$fatMin
     & protein < macros.list.single$proteinMax
     & protein > macros.list.single$proteinMin
     & carbs < macros.list.single$carbsMax
     & carbs > macros.list.single$carbsMin){
    return(T)
  } else {
    return(F)
  }
  
}

nyways.macros <- function(macros.list, foodProfile.list){
  # compares a food's nutrition data against a provided list of macro profiles
  
  
}

# Calculating Nutriscore ####
capValue <- function(value, cap){
  return(ifelse(value > cap, cap, value))
}

# MODIFIED SCORING SYSTEM
nutriscore.beta <- function(kcals,
                            sugar,
                            satFat,
                            sodium,
                            pctVeggies,
                            fiber,
                            protein,
                            liquidFood = FALSE){
  # the following numbers should each be per 100g of product
  kJ <- kcals*1000*0.004184

  # now assign points as per nutriscore system
  n_kcals = capValue(floor(kJ/335), 10)
  n_sugar <- capValue(floor(sugar/4.5), 10)
  n_satFat <- capValue(floor(satFat/1), 10)
  n_sodium <- capValue(floor(sodium/90), 10)
  
  n_veggies <- ifelse(
    pctVeggies > 0.8, 5, ifelse(
      pctVeggies > 0.6, 2, ifelse(
        pctVeggies > 0.4, 1, 0
        )
      )
    )
  n_fiber <- capValue(floor(fiber/0.9), 5)
  n_protein <- capValue(floor(protein/1.6), 5)
  
  nutriscore_raw <-  n_kcals + n_sugar + n_satFat + n_sodium - n_fiber - n_protein - n_veggies
  
  # BASIC ADAPTATION FOR LIQUIDS (given their content per serving, etc)
  if(liquidFood){
    nutriscore_raw <- nutriscore_raw + 10
  }

  nutriscore_rank <- ifelse(
    nutriscore_raw <= -1, "+++", ifelse(
      nutriscore_raw <= 5, "++", ifelse(
        nutriscore_raw <= 10, "+", ifelse(
          nutriscore_raw <= 18, "-","--"
        )
      )
    )
  )
  
  results.list <- list(
    kcals = n_kcals,
    kJ = kJ,
    sugar = n_sugar,
    satFat = n_satFat,
    sodium = n_sodium,
    fiber = n_fiber,
    protein = n_protein,
    nutriscore = nutriscore_raw,
    nutriscoreRank = nutriscore_rank
  )
  
  return(results.list)
  
}

# ORIGINAL SCORING SYSTEM
nutriscore.original <- function(kcals,
                            sugar,
                            satFat,
                            sodium,
                            pctVeggies,
                            fiber,
                            protein){
  # the following numbers should each be per 100g of product
  kJ <- kcals*1000*0.004184
  
  # now assign points as per nutriscore system
  n_kcals = capValue(floor(kJ/335), 10)
  n_sugar <- capValue(floor(sugar/4.5), 10)
  n_satFat <- capValue(floor(satFat/1), 10)
  n_sodium <- capValue(floor(sodium/90), 10)
  
  n_veggies <- ifelse(
    pctVeggies > 0.8, 5, ifelse(
      pctVeggies > 0.6, 2, ifelse(
        pctVeggies > 0.4, 1, 0
      )
    )
  )
  n_fiber <- capValue(floor(fiber/0.9), 5)
  n_protein <- capValue(floor(protein/1.6), 5)
  
  nutriscore_raw <-  n_kcals + n_sugar + n_satFat + n_sodium - n_fiber - n_protein - n_veggies
  
  nutriscore_rank <- ifelse(
    nutriscore_raw <= -1, "A", ifelse(
      nutriscore_raw <= 2, "B", ifelse(
        nutriscore_raw <= 10, "C", ifelse(
          nutriscore_raw <= 18, "D","E"
        )
      )
    )
  )
  
  results.list <- list(
    kcals = n_kcals,
    kJ = kJ,
    sugar = n_sugar,
    satFat = n_satFat,
    sodium = n_sodium,
    fiber = n_fiber,
    protein = n_protein,
    nutriscore = nutriscore_raw,
    nutriscoreRank = nutriscore_rank
  )
  
  return(results.list)
  
}

# COMBINE ALL SCORES ####
nyways.macros.allScores <- function(
  name,
  g_fat,
  g_carbs,
  g_protein,
  g_sugar,
  g_sugarAlcohol = 0,
  g_satFat,
  g_fiber,
  mg_sodium,
  kcals,
  g_serving,
  pctVeggies = 0
){
  
  tempMacroSplit <- nyways.macros.calculate.macroSplit(g_fat, 
                                                       g_carbs, 
                                                       g_protein)
  tempScores <- nyways.macros.calculate.basicScores(g_fat,
                                                    g_carbs,
                                                    g_protein,
                                                    g_sugar,
                                                    g_sugarAlcohol,
                                                    g_fiber,
                                                    kcals,
                                                    g_serving)
  tempNutriscore <- nutriscore.beta(kcals = kcals,
                                    sugar = g_sugar,
                                    satFat = g_satFat,
                                    sodium = mg_sodium,
                                    pctVeggies = pctVeggies,
                                    fiber = g_fiber,
                                    protein = g_protein)
  
  
  
  results.df <- data.frame(
    name = name,
    nutriScore = tempNutriscore$nutriscoreRank,
    proteinLvl = tempScores$proteinScore,
    fat = displayAsPct(tempMacroSplit$fat_pct),
    carbs = displayAsPct(tempMacroSplit$carbs_pct),
    netCarbs = tempScores$netCarbs,
    fat_100g = g_fat,
    carbs_100g = g_carbs,
    protein_100g = g_protein,
    sugar_100g = g_sugar,
    fiber_100g = g_fiber,
    nutriScoreRaw = tempNutriscore$nutriscore,
    proteinPer100kcal = tempScores$proteinRatio
  )
  
  return(results.df)
}


