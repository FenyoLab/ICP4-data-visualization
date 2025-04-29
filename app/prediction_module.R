library(bslib)
library(shinyWidgets)
library(shinyjs)

prediction_ui <- function(id) {
  
  ns <- NS(id)

    #textInput("textbox", "", ""),
    #actionButton("search","Search", style='padding:4px; font-size:80%')
  
  tagList(
    tags$head(
      useShinyjs(),
        tags$style(HTML("
            .shiny-notification {
                position: fixed;
                top: 50%;
                left: 50%;
                transform: translate(-50%, -50%);
                width: 300px;
            }
        "))
    ),
    fluidRow(
      style = "display: flex; margin-bottom: 20px; align-items: stretch;",
      column(4, 
        style = "display: flex; width: 25%;",
        card(
          style = "width: 100%; display: flex; flex-direction: column;",
          height = "auto",
          card_header("Prediction Algorithms"),
          p("We aggregated signal peptide predictions from 6 algorithms:"),
          #<li> <a href='https://services.healthtech.dtu.dk/services/SecretomeP-1.0/' target='_blank'>SecretomeP-1.0</a> </li>
          #<li> <a href='https://services.healthtech.dtu.dk/services/TMHMM-2.0/' target='_blank'>TMHMM-2.0</a> </li> 
          HTML("
            <ul>
              <li> <a href='https://topcons.net/' target='_blank'>TopCons</a> </li>
              <li> <a href='https://phobius.sbc.su.se' target='_blank'>Phobius</a> </li>
              <li> <a href='http://www.predisi.de/home.html' target='_blank'>Predisi</a> </li>
              <li> <a href='https://services.healthtech.dtu.dk/services/SignalP-4.1/' target='_blank'>SignalP-4.1</a> </li>
              <li> <a href='http://www.outcyte.com/analyse/' target='_blank'>Outcyte</a> </li>
              <li> <a href='https://busca.biocomp.unibo.it/deepsig/' target='_blank'>DeepSig</a> </li>
              
            </ul>
          "),
          p(HTML("The final Score is the weighted sum of these 6 algorithms. 
             The weights are determined by the accuracy of each algorithm on a ground truth dataset. 
             Details can be found <a href='SecretionPrediction.html' target='_blank'>here</a>."))
        )
      ),
      column(8,
        style = "display: flex; width: 75%;",
        card(
          style = "width: 100%; display: flex; flex-direction: column;",
          height = "auto",
          card_header("Secretion Prediction Search"),
          layout_columns(
            selectInput(ns("predction_id_type"), "Select ID Type:", 
                       choices = c("ensembl_gene_id", "ensembl_peptide_id", "uniprot", "gene_name")),
            layout_columns(
              textAreaInput(
              inputId = ns("search"),
              label = "Enter gene ids to search:",
              placeholder = "Type comma-separated ids here. e.g. ENSMUSG00000060816,ENSMUSG00000042446",
              width = "100%",
              rows = 1),
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                div(
                  style = "flex-grow: 1;",
                  fileInput(ns("file"), "Or upload a file:", 
                            accept = c(".csv", ".tsv", ".txt"),
                            width = "100%")
                ),
                actionButton(ns("clear_file"), NULL,
                            style = "height: 48px; width: 96px; margin-top: 0px; position: relative; top: 0px; display: flex; align-items: center; justify-content: center;",
                            icon = icon("xmark"))
              ),
              p("Uploaded files should have a header row. First column of the file will be used as gene ids matching the id type selected, regardless of the header name."),
              col_widths = c(12, 12, 12, 12)
            ),
            col_widths = c(3, 9)
          ),

          div(
            style = "margin-top: 10px; display: flex; justify-content: center;",
            actionButton(
              inputId = ns("btn_search"),
              label = "Search",
              icon = icon("magnifying-glass"),
              style = "margin-right: 5px;"
            ),
            actionButton(
              inputId = ns("btn_reset"),
              label = "Clear",
              icon = icon("xmark")
            )
          )
        )
      )
    ),
    fluidRow(
      column(12,
        uiOutput(ns("results_card"))
      )
    )
  )
}

prediction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print(paste0("Starting server module with id: ", id))  # Debug print
    
    # Define reactive values
    query_id_type <- reactiveVal(NULL)
    query_ids <- reactiveVal(NULL)
    query_results <- reactiveVal(NULL)

    # observe id_type and parse search text input
    observeEvent(input$predction_id_type, {
      print("predction_id_type changed")
      print(paste("New value:", input$predction_id_type))
      query_id_type(input$predction_id_type)
      print(paste("Stored value:", query_id_type()))
    })

    search_term <- reactiveVal(NULL)
    text_term <- reactiveVal(NULL)
    file_term <- reactiveVal(NULL)
    
    observeEvent(input$search,{
      req(input$search)
      text_term(strsplit(input$search, ",")[[1]])
    })

    observeEvent(input$file, {
      req(input$file)
      print(input$file)
      
      ext <- tools::file_ext(input$file$name)
      
      # Read file based on extension with better error handling
      tryCatch({
        df <- switch(ext,
          csv = read.csv(input$file$datapath, stringsAsFactors = FALSE),
          tsv = read.delim(input$file$datapath, stringsAsFactors = FALSE),
          txt = read.delim(input$file$datapath, stringsAsFactors = FALSE, fill = TRUE),  # Added fill=TRUE
          {
            showNotification("Unsupported file type. Please upload a .csv, .tsv, or .txt file.", type = "error")
            return(NULL)
          }
        )
        
        # Get first column
        if(ncol(df) > 0) {
          file_ids <- df[[1]]  # Get first column
          file_ids <- file_ids[!is.na(file_ids) & nzchar(trimws(file_ids))]  # Better empty check
          file_term(as.character(file_ids))
        } else {
          showNotification("File appears to be empty or improperly formatted.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        return(NULL)
      })
    })

    observeEvent(input$clear_file, {
          shinyjs::reset("file")
    })

    # get prediction query results when btn_search is clicked
    observeEvent(input$btn_search, {
      print("Search button clicked")
      
      # Combine terms safely
      text_terms <- if(!is.null(text_term())) text_term() else character(0)
      file_terms <- if(!is.null(file_term())) file_term() else character(0)
      combined_terms <- c(text_terms, file_terms)
      
      # Validate input
      if (length(combined_terms) == 0 || all(trimws(combined_terms) == "")) {
        showNotification(
          "Please enter at least one search term or upload a file of gene ids.",
          type = "warning"
        )
        return()
      }
      
      search_term(combined_terms)
      
      # Clean the IDs
      ids <- search_term()
      ids <- trimws(ids)  # Remove whitespace
      ids <- ids[nzchar(ids)]  # Remove empty strings
      
      if (length(ids) == 0) {
        showNotification(
          "Please enter valid search terms",
          type = "warning"
        )
        return()
      }
      
      print(paste("Current ID type:", query_id_type()))
      
      if (query_id_type() == "gene_name") {
        # combined queries with all capital letters and first letter capitalized
        ids = c(toupper(ids), paste0(toupper(substring(ids, 1, 1)), substring(ids, 2)))
      }
      else{
        # capitalize all letters
        ids = toupper(ids)
      }
      
      query_ids(ids)
      print(query_ids())
      
      withProgress(message = 'Retrieving data...', {
        results = get_prediction_data(id_type = query_id_type(), ids = query_ids())
      })
  
      if (is.null(results)) {
        showNotification(
          "Query failed. Please try again.",
          type = "warning"
        )
      }
      else if (is.character(results[[1]])){
        showNotification(
          paste( results[[1]],"Please double check the id type.", sep = "\n"),
          type = "warning"
        )
      }
      else{
        parsed = parse_prediction_data(results)
        query_results(parsed)
        output$query_results <- renderDT({
          datatable(query_results(),
            extensions = 'Buttons',
            options = list(
              pageLength = 20,
              dom = 'Bfrtip',
              buttons = list(
                list(
                  extend = 'csv',
                  filename = 'prediction_results'
                ),
                list(
                  extend = 'excel',
                  filename = 'prediction_results'
                )
              ),
              autoWidth = TRUE,
              scrollX = TRUE,
              scrollY = FALSE,
              columnDefs = list(
                list(targets = '_all', className = 'dt-center')
              ),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().container()).css({'font-size': '14px'});",
                "$('div.dataTables_wrapper table').css('margin', '0');",
                "$('.dataTables_wrapper tr').css('line-height', '1.4');",
                "$('.dataTables_wrapper').css('overflow', 'visible');",
                "this.api().columns.adjust();",
                "}")
            ),
            class = 'compact stripe',
            rownames = FALSE
          )
        })
      }
    
    })

    # clear query results when btn_reset is clicked
    observeEvent(input$btn_reset, {
      output$query_results <- NULL
      query_ids(NULL)
      query_results(NULL)
      text_term(NULL)  # Clear the text term
      file_term(NULL)  # Clear the file term
      search_term(NULL)  # Clear the combined search term
      updateTextAreaInput(session, "search", value = "")  # Clear the text input
      shinyjs::reset("file")    # Clear the file input
    })

    # Also clear file_term when clear_file button is clicked
    observeEvent(input$clear_file, {
      shinyjs::reset("file")
      file_term(NULL)  # Clear the file term
    })

    # Add back the results_card rendering
    output$results_card <- renderUI({
      req(query_results())
      card(
        style = "width: 100%; height: auto; overflow: visible; padding-bottom: 20px;",
        card_header("Query Results"),
        div(
          style = "overflow: visible; width: 100%;",
          DTOutput(ns("query_results"))
        )
      )
    })


  })
}
