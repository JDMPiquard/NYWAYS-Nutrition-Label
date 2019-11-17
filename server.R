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

	scannedUPC <- reactiveVal("")

	updateTable <- function(UPC){
		temp.df <- app.giveScoresFromUPC(UPC)
		if(resultsTable.df() != ""){
			tempResult <- rbind(resultsTable.df(), temp.df)
			resultsTable.df(tempResult)
		}else{
			resultsTable.df(temp.df)
		}
	}

	checkUPC <- function(UPC){
		return(ifelse(nchar(as.character(UPC))==12, T, F))
	}

	observeEvent(
		input$searchButton,
		{
			updateTable(input$searchText)
		}
	)

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
				updateTable(input$quaggaData)
			}
		}
	)

	



}