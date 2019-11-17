# USDA Scripts

require(DT)

# TEMP SCRIPTS ####

# fcid 595174
# API Key bKtg1jeCR4rtJznJL02OwVEsXowoecbbuKkwClHC

tempSearch <- usda.search("bulletproof", "bKtg1jeCR4rtJznJL02OwVEsXowoecbbuKkwClHC")

tempDetails <- usda.details("595174", "bKtg1jeCR4rtJznJL02OwVEsXowoecbbuKkwClHC")

# TEMP DATA DISPLAY ####
# search results
datatable(simple_list_to_df(tempSearch$foods))
datatable(tempNewDetails_parsed$nutrients)
# use the following to display the chosen result https://stackoverflow.com/questions/28274584/get-selected-row-from-datatable-in-shiny-app
# and https://rstudio.github.io/DT/shiny.html

# nutrients table per 100g of product
tempNutrients.df <- simple_list_to_df(tempDetails$foodNutrients)
datatable(tempNutrients.df)


tempLabel.list <- tempDetails$labelNutrients

