### NYWAYS Label display server



# Libraries and dependencies ####
require(shiny)
require(shinydashboard)
require(googleVis)
require(DT)
require(shinyjs)
source("lib/app-master-nyways-food-label.R")
source("ui/ui_food.R")
# source("ui/ui_form_sidebar.R")
# source("ui/ui_main_body.R")


function(input, output, session){

	# define the reactive values
	foodDetails.list <- reactiveVal("")

	resultsTable.df <- reactiveVal("")

	scannedUPC <- reactiveVal("")

	# MAIN [Non-Reactive] ####
	# set up the main functions to update the data display and calculations
	updateTable <- function(temp.df){

		if(resultsTable.df() != ""){
			tempResult <- rbind(resultsTable.df(), temp.df)
			resultsTable.df(tempResult)
		}else{
			resultsTable.df(temp.df)
		}
	}

	renderNylabelUI <- function(){
		temp.list <- foodDetails.list()
		if(temp.list != ""){
		  
		  # 1 create the UI
			output$ui_foodResults <- renderUI({
			  ui.SingleResults(title = paste0(temp.list$brandOwner, ": ", temp.list$description))
			})

			# update the reactive components within it
			output$ui_IngredientsTable <- DT::renderDataTable({
				datatable(
					temp.list$prettyIngredients,
					options = list(pageLength = 10,
						scrollX = TRUE), 
					rownames=F, 
					escape=F)
				
			}) 
		}
	}

	updateAll <- function(UPC) {

		tempDetails.list <- app.getFullDetailsFromUPC(UPC)
		foodDetails.list(tempDetails.list)
		
		# render the ui
		renderNylabelUI()
		updateTable(tempDetails.list$allScores)

	}

	checkUPC <- function(UPC){
		return(ifelse(nchar(as.character(UPC))==12, T, F))
	}

	# EVENT OBSERVERS ####
	observeEvent(
		input$searchButton,
		{
			updateAll(input$searchText)
		}
	)

	observeEvent(
		input$searchScanner,
		{
			insertUI(
				"#scanBox", 
				where = "beforeBegin", 
				div(id='scanBox1'))
			runjs('
				Quagga.init({
				    inputStream : {
				      name : "Live",
				      type : "LiveStream",
				      target: document.querySelector("#scanBox1")
				    },
				    decoder : {
				      readers : ["upc_reader", "upc_e_reader"]
				    }
				  }, function(err) {
				      if (err) {
				          console.log(err);
				          return
				      }
				      console.log("Initialization finished. Ready to start");
				      Quagga.start();
				      Quagga.onDetected(function(data){Shiny.setInputValue("quaggaData", data.codeResult.code);})
				  });')
		}
	)

	observeEvent(
		input$quaggaData,
		{
			scannedUPC(input$quaggaData)
			output$verbatim <- renderText({ input$quaggaData })
			removeUI("#scanBox1")
			runjs("Quagga.stop()")
			if(checkUPC(input$quaggaData)){
				updateAll(input$quaggaData)
			}
		}
	)

	# UI UPDATES [Reactive] ####
	output$UPCResult <- DT::renderDataTable({
		if(resultsTable.df() != ""){
			return(
			datatable(
				resultsTable.df(),
				options = list(pageLength = 30,
					scrollX = TRUE), 
				rownames=T, 
				escape=F)
		)
		}
	})


}