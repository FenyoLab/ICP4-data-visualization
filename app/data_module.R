library(bslib)
library(xdvir)
source('plot_module.R')

data_ui <- function(id) {
  withMathJax()
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
          div(class="d-flex w-100",
              uiOutput(ns("experiment_links"),
                       class="me-auto"),
              div( #right side of div with form inputs
                style = "min-width: 200px; padding-left: 20px; border-left: 1px solid #ddd; 
                 margin-left: auto;",
                h5("Quick molecule search"),
                selectInput(ns("id_type"), "Select ID Type",
                            choices = c(
                              "Gene Name" = "gene_name", #first is text in drop down, second is actual value submitted in form
                              "Ensembl Gene ID" = "ensembl_gene_id"
                            ),
                            selected = "gene_name"
                ),
                selectizeInput(ns("gene_search"), "Genes to search", choices=c(), multiple=T),
                numericInput(ns("p_val_gene_grid"),"p-value cutoff",0.05),
                numericInput(ns("lfc_cutoff_gene_grid"), "Log fold cutoff",2),
                div(
                  style = "display: flex; gap: 10px;",
                  actionButton(ns("search_btn"), "Search", icon = icon("magnifying-glass")),
                  actionButton(ns("clear_btn"), "Clear", icon = icon("xmark"))
                )
              )
          )
          #uiOutput(ns("experiment_links"))
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
         
         .link{
            height: 20px;
            display: flex;
            align-items: center; 
            padding-right: 20px;
            margin: 2px 0;
         }
      "))
    )
    #-----END STYLES-----
  )
}

data_server <- function(id, main_session, experiments) { #dir names of experiments in "experiments" folder, format <exp>_comparisons
  moduleServer(id, function(input, output, session) {
    
    #session$sendCustomMessage("mathjax-refresh", list())
    ns <- session$ns
    
    #-----INITILIZE REACTIVE VALUES-----
    
    experiment_data <- reactiveValues(
      name = NULL,
      all_files = NULL, #list of all experiment txt files (without ext)
      all_files_full_path = NULL
    )
    # Reactive values to store active experiment cards and selected experiment
    active_card <- reactiveVal(NULL)
    created_tabs <- reactiveValues(tabs = list())
    links <- reactiveValues(observers = list())
    cards <- reactiveValues(observers = list())
    previously_selected <- reactiveValues(exps = list())
    search_results <- reactiveVal(NULL)
    gene_id_to_name <- reactiveVal(NULL)
    
    
    #-----END INITIALIZE REACTIVE VALUES-----
    
    #-----PAGE RENDER SECTION-----
    
    output$experiment_links <- renderUI({ #basically left column
      # Left column with experiment links
      req(experiment_data$all_files) 
      div(
        style = "min-width: fit-content; overflow-x:auto;", #display flex
        uiOutput(ns("left_column")),
        tags$script(HTML("
           Shiny.addCustomMessageHandler('equalizeWidths', function(id) {
             var container = document.getElementById(id);
             var items = container.children;
             var maxWidth = 0;
             
             for (var i = 0; i < items.length; i++) {
               maxWidth = Math.max(maxWidth, items[i].offsetWidth);
             }
             
             for (var i = 0; i < items.length; i++) {
               items[i].style.width = maxWidth + 'px';
             }
           });
           
        "))
      )
      #)
    })
    
    output$left_column <- renderUI({
      tags$ul( #list of all exp
        style = "margin: 0; padding: 0; list-style-type: none;",
        tags$li( #column headers
          div(
            class="d-flex align-items-center",
            div(style="display:block; min-width:225px; height:20px;"),
            div(style = "flex:1; height:20px;",
                if(!is.null(search_results())){
                  div( #gene grid column headers
                    id = "column-headers",
                    style = "display:flex; justify-content:space-around; align-items:center ",#gap: 40px; margin-bottom: 10px;grid-auto-columns: auto; grid-auto-flow: column;
                    lapply(rownames(search_results()), function(gene) {
                      div(
                        style = #"display: flex; justify-content: space-between; align-items: center;
                          "min-width: fit-content; padding: 0 10px; height: 20px; font-weight: bold; text-align:center;",
                        gene
                      )
                    })
                  )
                }
            ),
          )
        ),
        lapply(rev(c(experiments)), function(exp_name) {
          exp_label <- strsplit(exp_name, "_")[[1]][1] #exp name without "_comparisons"
          tags$li( #exp list item
            class="pt-3",
            HTML(paste("<b><u>",exp_label,"</u></b>")),
            div(
              class="d-flex",
              div(
                style="width:225px;",
                tags$ul(#list of all files under each exp
                  lapply(list_txt_files(exp_name),function(file_name){
                    display_name <- strsplit(file_name,"-")[[1]][1:2]
                    #browser()
                    for (i in 1:length(display_name)){
                      if (display_name[i] == "n6") {
                        display_name[i] <- "&Delta;NTA"#"\\(\\Delta\\mathrm{NTA}\\)"
                      } else if (display_name[i] == "n208") {
                        display_name[i] <- "&Delta;CTA"#\\(\\Delta\\mathrm{CTA}\\)"
                      }
                    }

                    display_name <- paste(display_name, collapse=" vs ")
                    display_name <- gsub("_"," ",display_name)
                      
                    #browser()
                    tags$li(class="link", #file list item
                            actionLink(ns(file_name),(HTML(display_name))))
                  })
                )
              ),
              div( style = "flex:1;",
                   if(!is.null(search_results())){
                     createGeneGrid(search_results(),exp_label)
                   }
              )
            )
          )
        }),
        #tags$script(HTML("MathJax.typeset();")),
        tags$li( #ChipSeq div
          class = "pt-3",
          HTML("<b><u>ICP4 ChipSeq 2hpi (HSV-1)</u></b>"),
          div(class="d-flex",
            div(style="width:225px;",
              tags$ul(
                tags$li( style="list-style-type: none;",
                  "Promoter/5’UTR peak(s)"
                )
              )
            ),
            div( style = "flex:1;",
               if(!is.null(search_results())){
                 createGeneGrid(search_results(),"ChipSeq")
               }
            )
          )
        ),
        # Legend section
        if(!is.null(search_results())){
          div(class="d-flex",
            div(class="w-100"),#filler div
            div(
              style = "display: flex; justify-content: center; column-gap: 20px; margin-top: 15px; flex-wrap:wrap; min-width:225px; margin-right:15px; margin-left:auto;",
              div(
                class = "legend-item",
                div( class = "dot",
                     style = "background-color: dodgerblue;"),
                "Downregulated"
              ),
              div(
                class = "legend-item",
                div( class = "dot",
                     style = "background-color: darkorange;"),
                "Upregulated"
              ),
              div(
                class = "legend-item",
                div( class = "dot",
                     style = "background-color: lightgray;"),
                "Not significant"
              ),
              div(
                class = "legend-item",
                div( class = "dot",
                     style = "background-color: slategray;"),
                "NA"
              ),
              div(
                class = "legend-item",
                div(class = "dot",
                    style = "background-color: limegreen;"),
                "Found"
              ),
              div(
                class = "legend-item",
                div(class = "dot",
                    style = "background-color: red;"),
                "Not found"
              )
            )
          )
        }
      )
    })
    
    # Render UI for cards
    output$exp_cards_container <- renderUI({
      req(active_card())
      active_card()
    })

    # -----REACTIVE ELEMENTS/HELPER FUNCTIONS TO FILTER EXPERIMENTS BY PRESENCE OF SPECIFED GENES-----
    
    observeEvent(input$search_btn, {
      req(input$gene_search, input$id_type) #get input and its type (gene name, id, etc)
      
      genes <- input$gene_search
      
      # presence_matrix is df where:
      # - row names are experiment IDs
      # - column names are gene IDs
      # - values are TRUE/FALSE for presence/absence
      full_folder_name <- paste0( c("experiments/", experiment_data$name, "_comparisons"), collapse="" )
      
      withProgress(message = "Searching datasets for selected genes..." , {
        #returns df with rownames as genes and colnames as experiments; values either TRUE/FALSE for gene presence
        presence_df <- search_molecules(input$id_type, genes, experiment_data$all_files_full_path, input$p_val_gene_grid, input$lfc_cutoff_gene_grid) 
        
        if (is.null(presence_df)) {
          search_results(NULL)
        }
        else{
          display_presence <- as.matrix(presence_df)
          search_results(display_presence)
        }
      })
      
    })
    
    # -----END FILTER FUNCTIONS-----
    
    # Helper function for the gene grid
    createGeneGrid <- function(gene_results,category) { #gene_results: rownames = genes, colnames = experiments
      exp_files <- colnames(gene_results)[grepl(category,colnames(gene_results))]
      if(category == "ICP4")
        exp_files <- exp_files[!grepl("mock",exp_files)]
      tryCatch({
        # Grid of dots
        lapply(exp_files, function(file) { #file = file_name (no ext, just as in presence matrix)
          #print(file) #ensuring files are in same order as list
          div( #table row
            style = "display: flex; justify-content: space-around;height: 20px; align-items: center; margin: 2px 0;",  # grid-auto-columns: auto; grid-auto-flow: column; 
            lapply(rownames(gene_results), function(gene) {
              lfc <- gene_results[gene, file]
              div( #table cell
                style = "display: flex; justify-content: center; align-items: center; 
                         min-width: 60px; padding: 0 10px; height: 20px;",
                div(
                  style = sprintf("width: 12px; height: 12px; border-radius: 50%%; 
                                 background-color: %s; opacity: 0.8;",
                                 switch(as.character(lfc), "-1" = "dodgerblue", "0" = "lightgray", "1" = "darkorange", "-2" = "red", "2" = "limegreen", "slategray")) #,downregulated, ns, or upregulated, not found, or NA (no p value)
                )
              )
            })
          )
        })
      },
      error = function(e) {
        # On error, return just the experiment links
        div(
          style = "color: red; margin-bottom: 10px;",
          paste("Error:", e$message)
        )
      })
    }
    
    # Utility function to generate a card UI dynamically
    generate_exp_card <- function(file_name) { #file_name no ext.
      print(paste("card:", file_name))
      display_name <- strsplit(file_name,"-")[[1]][1:2]
      for (i in 1:length(display_name)){
        if (display_name[i] == "n6") {
          display_name[i] <- "&Delta;NTA"
        } else if (display_name[i] == "n208") {
          display_name[i] <- "&Delta;CTA"
        }
      }
      
      display_name <- paste(display_name, collapse=" vs ")
      display_name <- gsub("_"," ",display_name)
      div(
        class = "mb-3",  # adds margin-bottom of size 3 (Bootstrap spacing)
        card(
          id = ns(paste0("card_", file_name)), 
          card_header(HTML(display_name)),
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


    # Helper function to insert or switch to a tab, also set focus of selected data file
    insert_or_switch_tab <- function(full_file_path){
      base_file_name <- gsub("\\.txt$", "", basename(full_file_path))
      #base_file_name <- gsub("-", "_", base_file_name) #used for CARD HEADER, TAB ID, CLOSE BUTTON ID, AND PLOT ID
      tab_id <- paste0("tab_", base_file_name)
      close_button_id <- paste0("close_", tab_id)
      plot_id <- paste0("plot_", tab_id) 
      display_name <- strsplit(base_file_name,"-")[[1]][1:2]
      #browser()
      for (i in 1:length(display_name)){
        if (display_name[i] == "n6") {
          display_name[i] <- "&Delta;NTA"#"\\(\\Delta\\mathrm{NTA}\\)"
        } else if (display_name[i] == "n208") {
          display_name[i] <- "&Delta;CTA"#\\(\\Delta\\mathrm{CTA}\\)"
        }
      }
      
      display_name <- paste(display_name, collapse=" vs ")
      display_name <- gsub("_"," ",display_name)

      if (!tab_id %in% names(created_tabs$tabs)) {
        new_tab <- tabPanel(
          title = HTML(display_name),
          value = tab_id,
          fluidRow(
            class = "mt-2 mb-4",  # mt-2 reduces top margin, mb-4 increases bottom margin
            column(9, h4(HTML(display_name), class = "mb-0")),  # mb-0 removes margin below h3
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
          name = base_file_name,
          plot_id = plot_id,
          module = plot_server(plot_id, experiments, full_file_path, gene_id_to_name()) #experiments was originally user_info
        )
        
        # Set up remove button observer for this tab
        observeEvent(input[[close_button_id]], {
          print(paste("Closing tab:", tab_id))
          
          # Get the plot module's local_selected and trigger same behavior as clear_genes
          if (!is.null(created_tabs$tabs[[tab_id]]$module)) {
            # Get the module instance
            module <- created_tabs$tabs[[tab_id]]$module
            
            # Trigger same behavior as clear_genes
            #module$local_selected$genes <- character(0)
            #module$local_selected$click <- character(0)
            #module$local_selected$typed <- character(0)
            
            # Then do the regular cleanup
            module$cleanup()
          }
          
          removeTab(session = session, inputId = "data_tabs", target = tab_id)
          created_tabs$tabs[[tab_id]] <- NULL
          updateTabsetPanel(session, "data_tabs", selected = "Overview")
        }, ignoreInit = TRUE)
        
      } else { # switching to new tab
        updateTabsetPanel(session, "data_tabs", selected = tab_id)
      }
    } # END insert_or_switch_tab----
    
    list_txt_files <- function(exp_name,full_path=FALSE){ #(no path), returns filtered list of files in directory (no ext.)
      # Find corresponding full folder name
      txt_files_path <- file.path("experiments", exp_name)
      
      if (dir.exists(txt_files_path)) {
        txt_files <- list.files(path = txt_files_path, pattern = "*\\.txt$", full.names = full_path) 
        txt_files <- txt_files[!grepl("(GSEA|logCPM)", txt_files)] #only get non-GSEA/logCPM data (only DEseq)
        txt_files <- txt_files[!grepl("Shrink", txt_files)]
        
        if(!full_path)
          txt_files <- gsub("\\.txt$","",txt_files)
        return(txt_files)
      }
    }

    # Observer for the Get Analysis Results button, open or switch to the tab
    observe({
      lapply(experiment_data$all_files_full_path, function(full_file_path) { # w/ ext.
        base_file_name <- gsub("\\.txt$","", basename(full_file_path))
        tab_id <- paste0("get_tab_", base_file_name)
        observeEvent(input[[tab_id]], {
          insert_or_switch_tab(full_file_path)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
    
    observe({
      if(is.null(experiment_data$all_files)){
        experiment_data$all_files <- unlist(lapply(experiments,list_txt_files))
        experiment_data$all_files_full_path <- unlist(lapply(experiments,list_txt_files,full_path=TRUE))
        
        gene_info <- c() #get all genes and their IDs across all experiment files
        for(file in experiment_data$all_files_full_path){
          df <- read.table(file, header=TRUE)
          gene_ids <- df[["gene_id"]]
          unlabeled_genes <- setdiff(gene_ids,names(gene_info))
          filtered_df <- df[df[["gene_id"]] %in% unlabeled_genes,]
          gene_info[filtered_df[["gene_id"]]] <- filtered_df[["gene"]]
        }
        gene_id_to_name(gene_info)
        
        req(experiment_data$all_files)
        lapply(experiment_data$all_files, function(file_name) { #no ext
          observeEvent(input[[file_name]], {
            #req(experiment_data$name)      # double-check we did not leave
            active_card(NULL)
            active_card(generate_exp_card(file_name)) # no ext
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0("remove_", file_name)]], { #no ext
            active_card(NULL)
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Add clear button observer
    observeEvent(input$clear_btn, {
      search_results(NULL)  # Reset the search results
      if(input$id_type == "gene_name"){
        updateSelectizeInput(session,"gene_search",server=T, choices=as.vector(gene_id_to_name()), selected=c())
      }
      else{
        updateSelectizeInput(session,"gene_search",server=T, choices=names(gene_id_to_name()), selected=c())
      }
    })
    
    observeEvent(input$id_type,{
      #print("creating selectize from id_type")
      #print(gene_id_to_name()[1:5])
      if(input$id_type == "gene_name"){
        updateSelectizeInput(session,"gene_search",server=T,choices=as.vector(gene_id_to_name()))
      }
      else{
        updateSelectizeInput(session,"gene_search",server=T,choices=names(gene_id_to_name()))
      }
    })
    
    
    observeEvent(search_results(), {
      session$onFlushed(function() {
        session$sendCustomMessage("equalizeWidths", "column-headers")
      }, once = TRUE)
    })
  })
}
