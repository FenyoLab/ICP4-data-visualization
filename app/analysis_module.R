library(bslib)
analysis_ui <- function(id) {
  
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header("Upload Data for Differential Analysis"),
      p('Use pre-implemented pipeline for differential analysis.')
      ),
    card(
      card_header("Upload Raw Data")
      ),
    card(
      card_header("Upload Meta Data")
    ),
    card(
      card_header("Specify Linear Model")
    )
  )
}


analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server logic for the Home tab here
  })
}