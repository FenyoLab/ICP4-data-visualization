library(base)
library(shiny)
library(plotly)
library(DT)

source('utils.R')

# Combined Plot Module: plot_module.R

plot_ui <- function(id){
  #dat_dir = '/gpfs/data/proteomics/projects/fat_secretome/pipeline/data/'
  #dat_dir = '/srv/shiny-server/data/'
  
  print(paste0("plot_ui id: ", id))
  ns <- NS(id)
  
  # Global styling applied at the top level
  tagList(
    tags$style(HTML("
      /* Ensure cards expand with content */
      .card {
        height: auto !important;
        overflow: visible !important;
      }
      
      /* Set a flexible height for the plot column */
      .plot-column {
        height: auto !important;
      }
      
      /* Additional style for plotly output to help with resizing */
      .plotly-output {
        height: auto;
      }
    ")),
    tags$head(
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
    
    card(
      card_header(h5('Differential Analysis')),
      fluidRow(
        column(4,
               selectInput(ns("comparison"), "Select Comparison:", 
                           choices = NULL),
               sliderInput(ns("p_cutoff"), "-log10(P) Cutoff:", 
                           min = 0, max = 10, value = 2),
               sliderInput(ns("lfc_cutoff"), "LFC Cutoff:", 
                           min = 0, max = 10, value = 2)   
        ),
        column(8, class = "plot-column",
               card(
                 style = "width: 100%; padding-top: 20px;",
                 plotlyOutput(ns("volcano_plot"), 
                             height = "calc(100vw * 0.35)",
                             width = "100%"
                 )
               )
        )
      ),
      fluidRow(
        column(4,
               selectInput(ns("sort_by"), "Sort Table By:", 
                           choices = c("Positive Log2FC", "Negative Log2FC", "P Value", "Adjusted P Value")),
               numericInput(ns("table_cutoff"), "Cutoff for Sorted: ",
                            value = 0),
               numericInput(ns("n_feature"), "Number of Features:", 
                            min = 0, max = 500, step = 1, value = 20)
        ),
        column(8, 
               DTOutput(ns("top_table"))  # Top table
        )
      )
    ),
    
    card(
      card_header(h5("Normalized Mean Log Intensity")),
      fluidRow(
        column(4, 
               textAreaInput(ns("click_gene_list"), "Selected Genes (comma-separated):", 
                             placeholder = "Click on points in volcano plot to add gene names here", rows = 3),
               selectInput(ns("id_type"), "id Type:", choices = NULL),
               textAreaInput(ns("typed_gene_list"), "Selected Genes (comma-separated):", 
                             placeholder = "Type the gene names here", rows = 3),
               actionButton(ns("get_genes"), "plot genes"),
               actionButton(ns("clear_genes"), "clear")
        ),
        
        column(8, class = "plot-column",
               plotlyOutput(ns("gene_heatmap"), height = 'auto') # Plotly heatmap output area
               #textOutput(ns("selected_genes_text"))
        )
      )    
    )
  )
}


plot_server <- function(id, user_info, exp_id, molecule_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a unique source ID for this module instance
    plot_source <- paste0("volcano_", id)
    
    # Create isolated reactive values for this module instance
    local_selected <- reactiveValues(
      click = character(0),
      typed = character(0),
      genes = character(0),
      is_active = TRUE,
      should_render_heatmap = FALSE,
      is_fresh_session = TRUE,  # Add flag for fresh session
      current_plot_ids = NULL   # Add new reactive value for current plotted IDs
    )
    
    # Handle text input restoration
    observeEvent(input$click_gene_list, {
      # Ignore if this is initial load or cleanup
      if (local_selected$is_fresh_session) {
        # Clear the text input if it has a value on fresh session
        if (!is.null(input$click_gene_list) && input$click_gene_list != "") {
          updateTextAreaInput(session, "click_gene_list", value = "")
        }
        return()
      }
    }, ignoreInit = FALSE)  # We want to catch the initial value
    
    # Function to clean up the module state
    cleanup <- function() {
      print("=== Cleanup Event ===")
      
      # First disable rendering and active state
      local_selected$should_render_heatmap <- FALSE
      local_selected$is_active <- FALSE
      local_selected$is_fresh_session <- TRUE  # Reset fresh session flag
      
      # Reset button tracking
      last_button_value(0)
      
      # Clear reactive values
      local_selected$click <- character(0)
      local_selected$typed <- character(0)
      local_selected$genes <- character(0)
      
      # Clear text inputs - force immediate update
      isolate({
        updateTextAreaInput(session, "click_gene_list", value = "")
        updateTextAreaInput(session, "typed_gene_list", value = "")
      })
      
      # Clear outputs
      output$gene_heatmap <- NULL
      output$volcano_plot <- NULL
      output$selected_genes_text <- NULL
      output$top_table <- NULL
      
      print("=== Cleanup Complete ===")
    }
    
    # Reset fresh session flag on first real interaction
    observeEvent(event_data("plotly_click", source = plot_source), {
      if (local_selected$is_fresh_session) {
        local_selected$is_fresh_session <- FALSE
      }
    }, ignoreInit = TRUE)
    
    # Listen for cleanup message
    observeEvent(session$input[["plot_cleanup"]], {
      if (!is.null(session$input[["plot_cleanup"]]) && 
          session$input[["plot_cleanup"]]$id == id) {
        cleanup()
      }
    })
    
    # Initialize empty text areas for this instance
    isolate({
      updateTextAreaInput(session, "click_gene_list", value = "")
      updateTextAreaInput(session, "typed_gene_list", value = "")
    })

    # Add a reactive value to track initialization
    rv <- reactiveValues(initialized = FALSE)
    
    # Get all comparisons in the given experiment from the backend
    all_comparison <- get_all_comparisons(exp_id, user_info)
    
    # Modify the comparison handling
    observeEvent(all_comparison$out, {
      if (!is.null(all_comparison$out)) {
        updateSelectInput(session, "comparison",
                         choices = names(all_comparison$out), 
                         selected = names(all_comparison$out)[1])
        rv$initialized <- TRUE
      }
    })
    
    comparison_data <- reactive({
      req(local_selected$is_active, rv$initialized, input$comparison)
      comp_id = all_comparison$out[input$comparison]
      res = get_comparison_data(comp_id, user_info, molecule_type)$out
      #molecule_table = query_output$molecule_table
      #print(colnames(res))
      res$log10p = -log10(res$pValue)
      res$log10q = -log10(res$qValue)
      res
    })

    # sample condition information for the experiment
    condition_data <- reactive({
      req(local_selected$is_active)
      res = get_sample_conditions(exp_id, user_info)
      print(dim(res$out))
      res$out
    })

    molecule_data <- reactive({
      req(local_selected$is_active)
      molecule_table = get_exp_molecule(exp_id, user_info, molecule_type)$out
      #colnames(molecule_table)[2] = 'gene_name'
      #print(head(molecule_table))
      molecule_table
    })

    # Update plot_data reactive to handle initialization
    plot_data <- reactive({
      req(local_selected$is_active, comparison_data(), molecule_data())
      res = comparison_data()
      #res = merge(res, molecule_data(), by = 'id', all.x = TRUE)

      # assign color dynamically, blue for down, red for up, gray for ns
      res$category = 'ns'
      res$category[res$log2FC>=input$lfc_cutoff&res$log10p>= input$p_cutoff] = 'up'
      res$category[res$log2FC<= -input$lfc_cutoff&res$log10p>= input$p_cutoff] = 'down'
      #print(paste('res dataframe: ', dim(res)))
      colnames(res)[3] = 'gene_name'
      #print(colnames(res))
      res
    })
    
    # Render the interactive Plotly scatter plot with dynamic coloring
    color_dict = c("down" = "dodgerblue", "ns" = "gray", "up" = "darkorange")
    output$volcano_plot <- renderPlotly({
      withProgress(message = 'Retrieving data...', {
        req(plot_data())  # Ensure we have data before trying to plot
      })
      withProgress(message = 'Generating plot...', {
        p <- plot_ly(
          plot_data(), 
          x = ~log2FC, y = ~log10p, key = ~display_id, text = ~paste(gene_name, display_id, sep = ":"),
          type = 'scatter', mode = 'markers',
          marker = list(size = 10, opacity = 0.6),
          color = ~category,
          colors = color_dict,
          source = plot_source  # Use unique source ID
        ) %>%
          layout(
            title = list(
              text = paste("<b>Volcano Plot of ", input$comparison, "</b>"),
              font = list(size = 24, weight = "bold"), 
              y = 0.98  # Adjust title position
            ),
            margin = list(t = 50),  # Add top margin for title
            xaxis = list(
              title = list(
                text = "Log Fold Change",
                font = list(size = 18)  # X-axis title font size
              ),
              tickfont = list(size = 14)  # X-axis tick labels font size
            ),
            yaxis = list(
              title = list(
                text = "-log10(P)",
                font = list(size = 18)  # Y-axis title font size
              ),
              tickfont = list(size = 14)  # Y-axis tick labels font size
            ),
            legend = list(
              font = list(size = 16)  # Legend font size
            ),
            clickmode = 'event'
          )
        
        event_register(p, 'plotly_click')
        p
      })
    })

    # id types by molecule types
    observeEvent(molecule_type, {
      if (molecule_type == 'Protein'){
        updateSelectInput(session, "id_type",
                         choices = c("Gene Name" = "gene_name",
                                     "Ensembl Gene ID" = "ensembl_gene_id",
                                     "Ensembl Peptide ID" = "ensembl_peptide_id",
                                     "Uniprot ID" = "uniprot_id"),
                         selected = "gene_name")
      } else if (molecule_type == 'RNA'){
        updateSelectInput(session, "id_type",
                         choices = c("Gene Name" = "gene_name",
                                     "Ensembl Gene ID" = "ensembl_gene_id"),
                         selected = "gene_name")
      }
    })
 
    table_data <- reactive({
      sort_col_dict = c("log2FC", "log2FC", "pValue", "qValue")
      names(sort_col_dict) = c("Positive Log2FC", "Negative Log2FC", "P Value", "Adjusted P Value")
      sort_col = sort_col_dict[input$sort_by]
      #print(sort_col)
      cutoff_val = input$table_cutoff
      n_val = input$n_feature
      
      all_rows = plot_data()[,!colnames(plot_data()) %in% c('id','display_id')]
      #print(colnames(comparison_data))
      if (input$sort_by == "Negative Log2FC"){
        table_subset = all_rows[all_rows[,sort_col] <= -cutoff_val,]
        table_subset = table_subset[order(table_subset[,sort_col]),]
        
      }else if(input$sort_by == "Positive Log2FC"){
        table_subset = all_rows[all_rows[,sort_col] >= cutoff_val,]
        table_subset = table_subset[order(-table_subset[,sort_col]),]
      }else{
        table_subset = all_rows[all_rows[,sort_col] <= cutoff_val,]
        table_subset = table_subset[order(table_subset[,sort_col]),]
      }
      if(dim(table_subset)[1]>=n_val){
        table_subset = table_subset[0:n_val, ]
      }
      table_subset
    })
    
    output$top_table <- renderDT({
      datatable(table_data(),  
                extensions = 'Buttons',  # Add buttons extension
                options = list(
                  pageLength = 10,
                  dom = 'Bfrtip',  # Add 'B' for buttons
                  buttons = list(  # Configure download buttons
                    list(
                      extend = 'csv',
                      filename = 'differential_analysis_results'
                    ),
                    list(
                      extend = 'excel',
                      filename = 'differential_analysis_results'
                    )
                  ),
                  autoWidth = TRUE,
                  searchHighlight = TRUE,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().container()).css({'font-size': '14px'});",
                    "}")
                ),
                rownames = F) %>%
        formatRound(columns = c("log2FC", "log10p", "log10q"), digits = 4) %>%
        formatRound(columns = c("pValue", "qValue"), digits = 6)
    })
    
    # Modify click observer to check active state
    observeEvent(event_data("plotly_click", source = plot_source), {
      req(local_selected$is_active)
      click_data <- event_data("plotly_click", source = plot_source)
      if (!is.null(click_data)) {
        new_molecule_id <- as.character(click_data$key)
          local_selected$click <- unique(c(local_selected$click, new_molecule_id))
          local_selected$click = na.omit(local_selected$click)
          updateTextAreaInput(session, "click_gene_list", 
                            value = isolate(paste(local_selected$click, collapse = ",")))
        }
    })
    
    # Use local_selected instead of selected in all other observers
    observeEvent(input$click_gene_list, {
      modified_click_genes <- strsplit(input$click_gene_list, ",")[[1]]
      modified_click_genes <- trimws(modified_click_genes)
      modified_click_genes <- unique(modified_click_genes[modified_click_genes != ""])
      local_selected$click <- modified_click_genes
    })
    
    observeEvent(input$typed_gene_list, {
      typed_genes <- strsplit(input$typed_gene_list, ",")[[1]]
      typed_genes <- trimws(typed_genes)
      typed_genes <- unique(typed_genes[typed_genes != ""])
      local_selected$typed <- unique(typed_genes)
    })

    # Query the molecule_data to get ids of both clicked and typed genes
    selected_ids <- reactive({
      req(local_selected$is_active, molecule_data())
      lookup = molecule_data()
      # print("Lookup table:")
      # print(head(lookup))
      # print(colnames(lookup))
      # print("ID type:")
      # print(input$id_type)
      
      # Handle clicked genes
      clicked_ids = lookup[as.character(lookup$display_id) %in% local_selected$click, ]
      print("Clicked IDs:")
      print(clicked_ids)
      
      # Handle typed genes - make case insensitive
      typed_genes_lower = tolower(local_selected$typed)
      lookup_col_lower = tolower(as.character(lookup[[input$id_type]]))
      typed_ids = lookup[lookup_col_lower %in% typed_genes_lower, ]
      print("Typed IDs:")
      print(typed_ids)
      
      ids = unique(c(as.character(clicked_ids$id), as.character(typed_ids$id)))
      selected_id_df = lookup[as.character(lookup$id) %in% ids, c("id", "gene_name", "display_id")]
      selected_id_df$id = as.character(selected_id_df$id)
      print("selected_id_df:")
      print(selected_id_df)

      list(ids = selected_id_df$id, gene_names = selected_id_df$gene_name, display_id = selected_id_df$display_id)
    })
    

    # Track previous button value
    last_button_value <- reactiveVal(0)

    # Create the heatmap output only when get_genes is clicked
    observeEvent(input$get_genes, {
      print("=== Get Genes Event ===")
      print(paste("Button value:", input$get_genes))
      print(paste("Last button value:", last_button_value()))
      print(paste("Should render heatmap:", local_selected$should_render_heatmap))
      
      # Skip if this is not a new button click
      if (is.null(input$get_genes) || input$get_genes <= last_button_value()) {
        print("Skipping - not a new button click")
        return()
      }
      
      last_button_value(input$get_genes)
      
      req(local_selected$is_active)
      local_selected$genes = unique(c(local_selected$click, local_selected$typed))
      
      # Update current_plot_ids only when button is clicked
      local_selected$current_plot_ids <- selected_ids()$ids
      
      if (length(local_selected$current_plot_ids) == 0) {
        showNotification("No matching genes found", type = "warning")
        output$gene_heatmap <- NULL
        return()
      }
      
      # Only set should_render_heatmap to TRUE on actual button clicks
      local_selected$should_render_heatmap <- TRUE
      
      # Get sample value data
      withProgress(message = 'Retrieving molecule value data...', {
        sample_data <- reactive({
          req(local_selected$should_render_heatmap)
          req(local_selected$is_active)
          req(selected_ids_final())  # Changed from selected_ids()
          req(condition_data())
          
          sample_data_ls = get_exp_molecule_values(exp_id=exp_id, 
                                                molecule_type=molecule_type, 
                                                molecule_ids=selected_ids_final()$ids,  # Changed from selected_ids()$ids
                                                user_info)$out
          rownames(sample_data_ls$res) = sample_data_ls$res$id
          print("sample_data_ls$res:")
          print(dim(sample_data_ls$res))
          # Keep as data frame when subsetting columns
          sample_data_ls$res <- as.data.frame(sample_data_ls$res[, colnames(sample_data_ls$res) != 'id', drop = FALSE])
            print("sample_data_ls$res drop id:")
            print(dim(sample_data_ls$res))
          sample_data_ls
      })
    })

      print("Creating heatmap...")
    output$gene_heatmap <- renderPlotly({
        withProgress(message = 'Preparing data...', {
          # Require both conditions
      req(local_selected$should_render_heatmap)
      req(local_selected$is_active)
          
          incProgress(0.2, detail = "Getting dimensions...")
          print("selected_ids_final()$ids:")
          print(selected_ids_final()$ids)
          nrow = length(selected_ids_final()$ids)
          ncol = nrow(condition_data())
          
          incProgress(0.4, detail = "Processing condition data...")
          
          # Get annotation columns (everything except sample_name and replicate_id)
          annotation_cols <- setdiff(colnames(condition_data()), c('sample_name', 'replicate_id', 'library_size'))
      
          # Create col_data with proper row names
          col_data <- data.frame(
            condition_data()[, annotation_cols, drop = FALSE],
            row.names = rownames(condition_data()),
            stringsAsFactors = FALSE
          )
          
          # Ensure matrix structure when creating heatmap_data
          heatmap_data <- as.matrix(sample_data()$res)
          print(dim(heatmap_data))
          print(colnames(heatmap_data))
          print(rownames(heatmap_data))
          if (nrow(heatmap_data) == 1) {
            # Explicitly set dimensions for single-row case
            dim(heatmap_data) <- c(1, ncol(heatmap_data))
            rownames(heatmap_data) <- rownames(sample_data()$res)
            colnames(heatmap_data) <- colnames(sample_data()$res)
          }
          
          heatmap_data <- heatmap_data[selected_ids_final()$ids, rownames(col_data), drop = FALSE]
          
          print("After subsetting:")
          print("Heatmap data dimensions:")
          print(dim(heatmap_data))
          
          colnames(heatmap_data) <- condition_data()$sample_name
          print(colnames(heatmap_data))
          if (nchar(colnames(heatmap_data)[1]) > 10){
            colnames(heatmap_data) <- gsub("\\..*$", "",colnames(heatmap_data))
          }
          rownames(heatmap_data) <- paste(selected_ids_final()$gene_names, selected_ids_final()$display_id, sep = "_")
          print(rownames(heatmap_data))
          print('display_id: ')
          print(selected_ids_final()$display_id)
          print('gene_names: ')
          print(selected_ids_final()$gene_names)

          # After subsetting, handle NA values and breaks calculation
          print("Data range before NA handling:")
          print(range(heatmap_data, na.rm = TRUE))
          
          # Remove any Inf/-Inf values
          heatmap_data[is.infinite(heatmap_data)] <- NA
          
          if (sample_data()$data_type == 'raw_intensity'){
            color_palette = colorRampPalette(c("white", "indianred"))(256)
            heatmap_data = log2(heatmap_data + 1)
            data_range <- range(heatmap_data, na.rm = TRUE, finite = TRUE)
            if (all(is.finite(data_range))) {
              max_abs <- max(data_range)
              breaks = seq(0, max_abs, length.out = 256)
            } else {
              # Fallback if no finite values
              breaks = seq(0, 1, length.out = 256)
            }
            data_type_title = paste('log2(', sample_data()$data_type, ' + 1)', sep = '')
          } else if(sample_data()$data_type == 'read_counts'){
            color_palette = colorRampPalette(c("white", "indianred"))(256)
            # logcpm calculation
            print('library_size:')
            print(sample_data()$library_size)
            heatmap_data = log2(sweep(heatmap_data, 2, condition_data()$library_size, '/') * 1e6 + 1)
            data_range <- range(heatmap_data, na.rm = TRUE, finite = TRUE)
            if (all(is.finite(data_range))) {
              max_abs <- max(data_range)
              breaks = seq(0, max_abs, length.out = 256)
            } else {
              # Fallback if no finite values
              breaks = seq(0, 1, length.out = 256)
            }
            data_type_title = 'logcpm (from read counts)'
          
          }else {
            data_range <- range(heatmap_data, na.rm = TRUE, finite = TRUE)
            max_abs <- max(abs(data_range))
            if (min(data_range) < 0) {
              color_palette = colorRampPalette(c("dodgerblue2", "white", "darkred"))(256)
              breaks = seq(-max_abs, max_abs, length.out = 256)
            } else {
              color_palette = colorRampPalette(c("white", "indianred"))(256)
              breaks = seq(0, max_abs, length.out = 256)
            }
            
            data_type_title = sample_data()$data_type
          }
          
          print("Final data range:")
          print(range(heatmap_data, na.rm = TRUE, finite = TRUE))
          print("Breaks:")
          print(range(breaks))
          
          incProgress(1.0, detail = "Calculating dimensions...")
          num_rows <- nrow(heatmap_data)
          height_px <- max(400, num_rows * 40)
        })
        
        withProgress(message = 'Generating heatmap...', {
        heatmaply(
          heatmap_data,
          col_side_colors = col_data,
          col_side_colors_border = TRUE,
          col_side_colors_line_col = "black",
          colors = color_palette,
          breaks = breaks,
          Rowv = F, Colv = F,
          grid_color = 'white',
          fontsize_row = 14,
          fontsize_col = 14,
          subplot_heights = c(2/(num_rows + 2), num_rows/(2 + num_rows)),
          height = height_px,
          showticklabels = c(FALSE, TRUE),  # (columns, rows)
          limits = NULL,
          main = data_type_title,
          ) %>%
            layout(
              xaxis = list(
                tickfont = list(size = 14, color = 'black'),
                showticklabels = FALSE  # Hide column labels
              ),
              yaxis = list(
                tickfont = list(size = 14, color = 'black'),
                showticklabels = TRUE   # Show row labels
              )
            )
        })
      })
    }, ignoreInit = TRUE)  # Add ignoreInit
    
    # Clear heatmap when clear_genes is clicked
    observeEvent(input$clear_genes, {
      local_selected$should_render_heatmap <- FALSE
      local_selected$genes <- character(0)
      local_selected$click <- character(0)
      local_selected$typed <- character(0)
      updateTextAreaInput(session, "click_gene_list", value = "")
      updateTextAreaInput(session, "typed_gene_list", value = "")
      
    })

    print(paste("Namespace:", session$ns("volcano_plot"))) 
    print('getting data')

    print(paste('molecule_type: ', molecule_type))

    # Create reactiveVal to store the last selected IDs
    last_selected_ids <- reactiveVal(NULL)
    
    # Update last_selected_ids when button is clicked
    observeEvent(input$get_genes, {
      last_selected_ids(selected_ids())
    })
    
    # Create reactive that only depends on last_selected_ids
    selected_ids_final <- reactive({
      last_selected_ids()
    })

    # Return module interface
    return(list(
      cleanup = cleanup,
      local_selected = local_selected
    ))
  })
}                                         