# ui for one by one food display

require(shiny)
require(shinydashboard)
require(DT)

ui.Ingredients <- function(){
	return(
		tagList(
			box(DT::dataTableOutput('ui_IngredientsTable'), 
				title = "Ingredients", 
				footer = NULL, 
				status = NULL,
				solidHeader = TRUE, 
				background = NULL, 
				width = 12, 
				height = NULL,
				collapsible = TRUE, 
				collapsed = FALSE)
		)
	)
	
}

ui.SingleResults <- function(title = ""){
	return(
		tagList(
			h2(paste(title, " - Nutrition Analysis")),
			ui.Ingredients()
		)
	)
}