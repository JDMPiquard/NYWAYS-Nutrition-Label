### NYWAYS Nutrition Lable display UI

# LIBRARIES ####
require(shiny)
require(shinydashboard) 
library(shinycssloaders)


dashboardPage(
  # HEADERS ####
  skin = "black",
  dashboardHeader(title = "NYWAYS Food Label"),
  
  # DASH SIDEBAR ####
  dashboardSidebar(
    disable = FALSE,
    collapsed = FALSE,
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search UPC")
    # uiOutput("ui_sidebar")
  ),
  
  # DASH BODY ####
  dashboardBody(
    # _ styling changes ####
    # tags$head(
    #   tags$style(
    #     HTML('
    #       /* edit sidebar toggle icon /*
    #       .main-header .sidebar-toggle:before {
    #                         content: "\\f044";
    #       }

    #       /* main sidebar */
    #                             .skin-black .wrapper {
    #          background-color: #f9f9f9;
    #                             }

    #       /* body */
    #                             .content-wrapper, .right-side {
    #          background-color: #f9f9f9;
    #          }
    #     '
    #     ))),

    fluidRow(
      column(12,
        h1("UPC Search")
      )
    ),
    fluidRow(
      column(12,
        (DT::dataTableOutput('UPCResult'))
      )
    )

    
    
    # _ main body output ####
    # withSpinner(uiOutput("ui_main_body"))
    
  )
)