### NYWAYS Label display server



# Libraries and dependencies ####
require(shiny)
require(shinydashboard)
require(googleVis)
require(DT)
require(shinyjs)
source("lib/app-master-nyways-food-label.R")
# source("ui/ui_form_sidebar.R")
# source("ui/ui_main_body.R")


function(input, output, session){

	resultsTable.df <- reactiveVal("")

	observeEvent(
		input$searchButton,
		{
			temp.df <- app.giveScoresFromUPC(input$searchText)
			if(resultsTable.df() != ""){
				tempResult <- rbind(resultsTable.df(), temp.df)
				resultsTable.df(tempResult)
			}else{
				resultsTable.df(temp.df)
			}
		}
	)

	output$UPCResult <- DT::renderDataTable({
		return(
			datatable(
				resultsTable.df(),
				options = list(pageLength = 30,
					scrollX = TRUE), 
				rownames=T, 
				escape=F)
		)
	})



}