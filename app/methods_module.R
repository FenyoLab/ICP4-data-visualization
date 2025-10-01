library(bslib)

methods_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header ("Methods"),
      p("Here will be a descriptions of the methods")
    )
  )
}

methods_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}