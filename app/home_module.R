library(bslib)

home_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header ("Summary"),
         
      p("The impacts of ICP4 on viral transcription have been well studied. The impacts of ICP4 on host transcription during HSV-1 
      infection have also been investigated, however no study has examined the impact of ICP4 expression in uninfected cells."),
    ),
  
    card(
      card_header("Links"),
      HTML("
      <ul>
        <li> <a href='https://fenyolab.org' target='_blank'>Download raw data on GEO</a> </li>
        <li> <a href='https://fenyolab.org' target='_blank'>Our publication</a> </li>
      </ul>
      ")
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server logic for the Home tab here
  })
}


