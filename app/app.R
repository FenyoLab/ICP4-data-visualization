library(shiny)
library(bslib)
library(httr)

source('login_module.R')
source('home_module.R')
source('data_module.R')
source('analysis_module.R')
source('prediction_module.R')

options(shiny.session.timeout = 0)

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    navbar_bg = "#337ab7"
  ),
  
  # Add padding to account for fixed navbar
  tags$head(
    tags$style(HTML("
      .tab-content {
        padding-top: 50px !important;
      }
      .navbar-brand {
        color: white !important;
        text-decoration: none !important;
        font-size: 22px !important;
      }
      .navbar-nav .nav-item .nav-link,
      .navbar-nav .nav-item .nav-link:link,
      .navbar-nav .nav-item .nav-link:visited {
        background-color: transparent;
        color: white;
        text-decoration: none !important;
        border: none !important;
        font-size: 16px !important;
      }
      .navbar-nav .nav-item .nav-link.active,
      .navbar-nav .nav-item .nav-link.active:hover {
        background-color: white !important;
        color: black !important;
        text-decoration: none !important;
        border: none !important;
        border-bottom: none !important;
      }
      .navbar-nav .nav-item .nav-link:hover,
      .navbar-nav .nav-item .nav-link:focus {
        background-color: #d9edf7;
        color: black;
        text-decoration: none !important;
        border: none !important;
      }
      /* Add this to push login to the right */
      .navbar-nav:last-child {
        margin-left: auto;
      }
    "))
  ),
  
  navbarPage(
    title = "Secrepedia",
    id = "main_navbar",
    position = "fixed-top",
    fluid = TRUE,
    
    # Main navigation tabs
    tabPanel("Home", home_ui("home")),
    tabPanel("Data", data_ui("data")),
    tabPanel("Prediction", prediction_ui("prediction")),
    #tabPanel("Analysis", analysis_ui("analysis")),
    
    # Login dropdown on the right
      login_ui("login")
    )
  )

server <- function(input, output, session) {
  # Initialize login module
  user_info <- login_server("login", session)

  # Call the data module server
  data_server("data", session, user_info)
  prediction_server("prediction")
  
}

# Run the Shiny App
shinyApp(ui, server)