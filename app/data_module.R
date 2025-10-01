library(bslib)
source('plot_module.R')

data_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("data_tabs"),
    tabPanel(
      "Overview",
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        card(
          card_header("Available Datasets"),
          uiOutput(ns("experiment_links"))
        ),
        uiOutput(ns("exp_cards_container"))
      )
    ),
    # Additional tabs will be inserted here
    
    #-----STYLES-----
    tags$head(
      tags$style(HTML("
         .legend-item{
            display: flex;
            align-items: center;
            gap: 5px;
            font-size: 0.8rem;
         }  
         
         .dot{
            width: 12px;
            height: 12px;
            border-radius: 50%;
         }
      "))
    )
    #-----END STYLES-----
  )
}

data_server <- function(id, main_session, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    experiment_data <- reactiveValues(
      name = NULL,
      txt_files = NULL,
      main_file = NULL,
      selected_files = NULL
    )
    
    output$experiment_links <- renderUI({
      # Create ordered experiment list once to ensure consistency

      experiments <- user_info$experiments
      #base_txt_files <- NULL

      div(
        style = "display: flex; align-items: start; width: 100%;",
        # Left column with experiment links
        if(is.null(experiment_data$name)){
          #print(experiment_data)
          div(
            style = "min-width: fit-content;",
            tags$div(style = "height: 25px;"),
            tags$ul(
              style = "margin: 0; padding: 0; list-style-type: none;",
              lapply(experiments, function(exp) {
                exp_label <- strsplit(exp, "_")[[1]][1]
                tags$li(
                  style = "height: 20px; display: flex; align-items: center; 
                         padding-right: 20px; margin: 2px 0;",
                  actionLink(ns(exp_label), exp)
                )
              })
            )
          )
        }
        else{ #experiment selected
          div(
            style = "min-width: fit-content;",
            actionLink(ns("back_to_experiments"), "← Back to Experiments", 
                       style = "color: #666;"),
            tags$div(style = "height: 25px;"),
            tags$ul(
              style = "margin: 0; padding: 0; list-style-type: none;",
              lapply(experiment_data$selected_files, function(file) { #file = file_name (no ext)
                tags$li(
                  style = "height: 20px; display: flex; align-items: center; 
                           padding-right: 20px; margin: 2px 0;",
                  actionLink(ns(file), file)
                )
              })
            )
          )
        }
        ,
        #Middle column with gene grid (when present)
        if (!is.null(search_results())){ #&& dim(search_results())[1] == length(experiments)) {
          div(
            style = "position: sticky; left: 0;",
            createGeneGrid(search_results())
          )
        },
        # Right column with search panel
        if(!is.null(experiment_data$name)){
          div(
            style = "min-width: 200px; padding-left: 20px; border-left: 1px solid #ddd; 
                   margin-left: auto;",
            h5("Quick molecule search"),
            selectInput(ns("id_type"), "Select ID Type",
                        choices = c(
                          "Gene Name" = "gene_name", #first is text in drop down, second is actual value submitted in form
                          "Ensembl Gene ID" = "ensembl_gene_id"
                          #"Ensembl Peptide ID" = "ensembl_peptide_id",
                          #"Uniprot ID" = "uniprot"
                        ),
                        selected = "gene_name"
            ),
            textInput(ns("gene_search"), "Values to search", 
                      placeholder = "Enter values (comma-separated)"),
            div(
              style = "display: flex; gap: 10px;",
              actionButton(ns("search_btn"), "Search", icon = icon("magnifying-glass")),
              actionButton(ns("clear_btn"), "Clear", icon = icon("xmark"))
            )
          )
        }
      )
    })
    
    # Reactive values to store active experiment cards and selected experiment
    active_cards <- reactiveValues(cards = list())
    created_tabs <- reactiveValues(tabs = list())
    #selected_exp <- reactiveVal(NULL)  # Store the selected experiment
    

    # -----REACTIVE ELEMENTS/HELPER FUNCTIONS TO FILTER EXPERIMENTS BY PRESENCE OF SPECIFED GENES-----
    search_results <- reactiveVal(NULL)
    
    observeEvent(input$search_btn, {
      req(input$gene_search, input$id_type) #get input and its type (gene name, id, etc)
      genes <- trimws(strsplit(input$gene_search, ",")[[1]]) 
      # Use the same ordered experiments
      #experiments <- user_info$experiments#[order(sapply(user_info$experiments, function(exp) exp$id))]
      #exp_ids_to_search <- as.character(sapply(ordered_experiments, function(exp) exp$id))
      #exp_ids <- tools::file_path_sans_ext(basename(file))
      
      #print(paste("exp_ids_to_search:", exp_ids_to_search))
      #print(paste("user_token NULL:", is.null(user_info$access_token)))
      
      # Assuming presence_matrix is a data frame where:
      # - row names are experiment IDs
      # - column names are gene IDs
      # - values are TRUE/FALSE for presence/absence
      full_folder_name <- paste0( c("experiments/", experiment_data$name, "_comparisons"), collapse="" )
      full_file_paths <- list.files(full_folder_name, pattern = "\\.txt$", full.names = TRUE)
      full_file_paths <- full_file_paths[!grepl("^(GSEA|logCPM)", full_file_paths)]
      
      presence_df <- search_molecules(input$id_type, genes, full_file_paths) #returns df with rownames as genes and colnames as experiments; values either TRUE/FALSE for gene presence
      #print(presence_matrix)
      # display warning message if colnames(presence_matrix) is different from genes
      if (is.null(presence_df) || length(rownames(presence_df)) < length(genes)) {
        showModal(modalDialog(
          title = "",
          "Some genes were either filtered out during quality control or not found in the database. Please check id type or spelling."
        ))
        experiment_data$selected_files <- gsub("\\.txt$","",experiment_data$txt_files)
      }
      
      if (is.null(presence_df)) {
        experiment_data$selected_files <- gsub("\\.txt$","",experiment_data$txt_files)
        search_results(NULL)
      }
      else{
        display_presence <- as.matrix(presence_df)
        experiment_data$selected_files <- colnames(presence_df)
        search_results(display_presence)
      }
    })
    
    #-----END FILTER FUNCTIONS-----
    
    # Add clear button observer
    observeEvent(input$clear_btn, {
      search_results(NULL)  # Reset the search results
      experiment_data$selected_files <- gsub("\\.txt$","",experiment_data$txt_files)
      updateTextInput(session, "gene_search", value = "")  # Clear the input field
    })
    
    # Helper function for the gene grid
    createGeneGrid <- function(gene_results) { #data files = file_name (no ext), only selected ones
      # Error handling wrapper
      tryCatch({
        tags$div(
          style = "display: inline-block;",
          # Gene names as column headers
          tags$div(style = "height: 25px;"),
          tags$div(
            style = "display: grid; grid-auto-columns: auto; grid-auto-flow: column; gap: 40px; margin-bottom: 10px;",
            # tags$div(style = "width: 40px;"),
            lapply(rownames(gene_results), function(gene) {
              tags$div(
                style = "display: flex; justify-content: center; align-items: center; 
                         min-width: 60px; padding: 0 10px; height: 20px; font-weight: bold;",
                gene
              )
            })
          ),
          # Grid of dots
          tags$div(
            lapply(colnames(gene_results), function(file) { #CHANGED FROM DATA FILES, file = file_name (no ext, just as in presence matrix)
              #print(file) #ensuring files are in same order as list
              tags$div(
                style = "display: grid; grid-auto-columns: auto; grid-auto-flow: column; gap: 40px; 
                         height: 20px; align-items: center; margin: 2px 0;",
                # tags$div(style = "width: 40px;"),
                lapply(rownames(gene_results), function(gene) {
                  lfc <- gene_results[gene, file]
                  tags$div(
                    style = "display: flex; justify-content: center; align-items: center; 
                             min-width: 60px; padding: 0 10px; height: 20px;",
                    tags$div(
                      style = sprintf("width: 12px; height: 12px; border-radius: 50%%; 
                                     background-color: %s; opacity: 0.8;",
                                     switch(as.character(lfc), "-1" = "dodgerblue", "0" = "lightgray", "1" = "darkorange", "-2" = "red",  "slategray")) #,downregulated, ns, or upregulated, not found, or NA (no p value)
                    )
                  )
                })
              )
            })
          ),
          # Legend section
          tags$div(
            style = "display: flex; justify-content: center; column-gap: 20px; margin-top: 15px; flex-wrap:wrap; min-width:225px; margin-right:15px",
            tags$div(
              class = "legend-item",
              tags$div( class = "dot",
                        style = "background-color: dodgerblue;"),
                "Downregulated"
            ),
            tags$div(
              class = "legend-item",
              tags$div( class = "dot",
                        style = "background-color: darkorange;"),
                "Upregulated"
            ),
            tags$div(
              class = "legend-item",
              tags$div( class = "dot",
                        style = "background-color: lightgray;"),
                "Not significant"
            ),
            tags$div(
              class = "legend-item",
              tags$div( class = "dot",
                        style = "background-color: slategray;"),
                "NA"
            ),
            tags$div(
              class = "legend-item",
              tags$div(class = "dot",
                       style = "background-color: red;"),
                "Not found"
            )
          )
        )
      },
      error = function(e) {
        # On error, return just the experiment links
        tags$div(
          style = "color: red; margin-bottom: 10px;",
          paste("Error:", e$message)
        )
      })
    }
    
    # Utility function to generate a card UI dynamically
    generate_exp_card <- function(file_name) { #file_name = file with ext
      print("generating card")
      base_file_name <- gsub("\\.txt$", "", file_name)
      div(
        class = "mb-3",  # adds margin-bottom of size 3 (Bootstrap spacing)
        card(
          id = ns(paste0("card_", file_name)), 
          card_header(base_file_name),
          fluidRow(
            column(6, div()),#show_exp_details(user_info, exp_id)),
            column(4, div(style = "text-align: right;",
              actionButton(ns(paste0("get_tab_", file_name)), "Get Analysis Results")
            )),
            column(2, div(style = "text-align: right;",
              actionButton(ns(paste0("remove_", file_name)), "Hide")
            ))
          )
        )
      )
    }
    
    # Render UI for cards
    output$exp_cards_container <- renderUI({
      do.call(tagList, active_cards$cards)
    })
    
     # Experiment onclick event to bring up txt file list under this experiment directory
    observe({
      experiments <- user_info$experiments
      lapply(experiments, function(exp){
        
        exp_title <- strsplit(exp, "_")[[1]][1] #exp folder names are of format exp_comparisons
        
        observeEvent(input[[exp_title]], {
          
          experiment_data$name <- exp_title #without "_comparison"
          
          # Find corresponding full folder name
          txt_files_path <- file.path("experiments", exp)
          
          if (dir.exists(txt_files_path)) {
            txt_files <- list.files(path = txt_files_path, pattern = "*\\.txt$", full.names = FALSE) 
            txt_files <- txt_files[!grepl("^(GSEA|logCPM)", txt_files)] #only get non-GSEA/logCPM data (only DEseq)
            experiment_data$txt_files <- txt_files
            base_txt_files <- gsub("\\.txt$","",txt_files)
            experiment_data$selected_files <- base_txt_files
          } else {
            experiment_data$txt_files <- NULL
            experiment_data$selected_files <- NULL
            #print("Directory not found or no txt files")
          }
        })
      })
    })

    # Helper function to insert or switch to a tab, also set focus of selected data file
    insert_or_switch_tab <- function(file_name){# file_name.txt, not full path
      experiment_data$main_file <- file_name
      base_file_name <- gsub("\\.txt$", "", file_name)
      base_file_name <- gsub("-", "_", base_file_name) #used for CARD HEADER, TAB ID, CLOSE BUTTON ID, AND PLOT ID
      tab_id <- paste0("tab_", base_file_name)
      close_button_id <- paste0("close_", tab_id)
      plot_id <- paste0("plot_", tab_id) 

      if (!tab_id %in% names(created_tabs$tabs)) {
        #print(paste("Creating new tab", tab_id))
        full_folder_name <- paste0(experiment_data$name, "_comparisons")
        full_file_path <- file.path("experiments", full_folder_name, file_name)
        new_tab <- tabPanel(
          title = paste("Experiment", base_file_name),
          value = tab_id,
          fluidRow(
            class = "mt-2 mb-4",  # mt-2 reduces top margin, mb-4 increases bottom margin
            column(9, h4(base_file_name, class = "mb-0")),  # mb-0 removes margin below h3
            column(3, actionButton(ns(close_button_id), "Close Tab"))
          ),
          plot_ui(ns(plot_id)) #plots log2FC data from file
        )
        
        insertTab(
          session = session,
          inputId = "data_tabs",
          tab = new_tab,
          target = "Overview",
          position = "after",
          select = TRUE
        )
        
        # Store the module instance
        created_tabs$tabs[[tab_id]] <- list( #tab_<base_file_name>
          name = file_name,
          plot_id = plot_id,
          module = plot_server(plot_id, user_info, full_file_path)
        )
        
        # Set up remove button observer for this tab
        observeEvent(input[[close_button_id]], {
          print(paste("Closing tab:", tab_id))
          
          # Get the plot module's local_selected and trigger same behavior as clear_genes
          if (!is.null(created_tabs$tabs[[tab_id]]$module)) {
            # Get the module instance
            module <- created_tabs$tabs[[tab_id]]$module
            
            # Trigger same behavior as clear_genes
            module$local_selected$should_render_heatmap <- FALSE
            module$local_selected$genes <- character(0)
            module$local_selected$click <- character(0)
            module$local_selected$typed <- character(0)
            
            # Then do the regular cleanup
            module$cleanup()
          }
          
          removeTab(session = session, inputId = "data_tabs", target = tab_id)
          created_tabs$tabs[[tab_id]] <- NULL
          updateTabsetPanel(session, "data_tabs", selected = "Overview")
        }, ignoreInit = TRUE)
        
      } else { # switching to new tab
        #print(paste("Switching to existing tab:", tab_id))
        updateTabsetPanel(session, "data_tabs", selected = tab_id)
      }
    }

    # Observer for the Get Analysis Results button, open or switch to the tab
    observe({
      txt_files <- experiment_data$txt_files
      lapply(txt_files, function(file_name) { #file_name.txt (not full path)
        tab_id <- paste0("get_tab_", file_name)
        observeEvent(input[[tab_id]], {
          insert_or_switch_tab(file_name)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
    
    # Implement back button
    observeEvent(input$back_to_experiments, {
      experiment_data$name <- NULL
      experiment_data$txt_files <- NULL
      experiment_data$selected_files <- NULL
      experiment_data$main_file <- NULL
      search_results(NULL)
      active_cards$cards <- list()
    })
    
    # File selection observer (same as before)
    observe({
      if (!is.null(experiment_data$txt_files)) {
        lapply(experiment_data$txt_files, function(file_name) { #file_name with ext. (not full path)
          base_file_name <- gsub("\\.txt$","",file_name)
          observeEvent(input[[base_file_name]], {
            #remove existing active card to show only 1 at a time
            active_cards$cards <- list()
            active_cards$cards[[file_name]] <- generate_exp_card(file_name)
          })
          
          # Set up remove button observer for this experiment
          observeEvent(input[[paste0("remove_", file_name)]], {
            active_cards$cards[[file_name]] <- NULL
          })
        })
      }
    })
  })
}
