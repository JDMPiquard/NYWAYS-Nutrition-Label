### NYWAYS Nutrition Lable display UI

# LIBRARIES ####
require(shiny)
require(shinydashboard) 
library(shinycssloaders)
require(shinyWidgets)
require(shinyjs)


dashboardPage(
  # HEADERS ####
  skin = "black",
  dashboardHeader(title = "NYWAYS Food Label"),
  
  # DASH SIDEBAR ####
  dashboardSidebar(
    useShinyjs(),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/quagga/0.12.1/quagga.min.js"),
    disable = FALSE,
    collapsed = FALSE,
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search UPC"),
    column(12, p("or scan the barcode")),
    actionBttn(
      inputId = "searchScanner",
      label = "Scan Code",
      color = "success", # should be one of “default”, “primary”, “warning”, “danger”, “success”, “royal”
      style = "jelly" # “simple”, “bordered”, “minimal”, “stretch”, “jelly”, “gradient”, “fill”, “material-circle”, “material-flat”, “pill”, “float”, “unite”
    )
    # ,button()
    # uiOutput("ui_sidebar")


  ),
  
  # DASH BODY ####
  dashboardBody(
    useShinyjs(),
    div(id='scanBox', ''),
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
        uiOutput("ui_foodResults")
      )
    ),
    fluidRow(
      column(12,
        h1("Comparison of Nutritional Analysis")
      )
    ),
    fluidRow(
      column(12,
        (DT::dataTableOutput('UPCResult')),
        verbatimTextOutput('verbatim')
      )
    )

    
    
    # _ main body output ####
    # withSpinner(uiOutput("ui_main_body"))
    
  )
)