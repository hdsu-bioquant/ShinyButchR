#------------------------------------------------------------------------------#
#                 Server Make K picker and Annot picker                        #
#------------------------------------------------------------------------------#
sel_KServer <- function(id, nmf_obj, annot_react) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Select K
      output$sel_K <- renderUI({
        #nmf_obj()
        #req(nmf_obj())
        
        #ns <- session$ns
        ns <- NS(id)
        ks <- nmf_obj()@OptKStats$k
        optk <- nmf_obj()@OptK
        
        selectInput(
          inputId = ns("sel_K"),
          #label = "Select factorization rank:",
          label = "Select K:",
          choices = ks,
          selected = ifelse(length(optk) == 0,
                            ks[1], max(optk)),
          multiple = FALSE
        )
      })
      
      
      # Selector columns to use
      output$inputannot_selcols <- renderUI({
        req(annot_react())
        ns <- NS(id)
        pickerInput(
          inputId  = ns("inputannot_selcols"),
          label    = "Select columns to use", 
          choices  = colnames(annot_react())[-1],
          selected = colnames(annot_react())[2],
          options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
        )
      })

    }
  )
}

#------------------------------------------------------------------------------#
#           UI  K picker and Annot picker & Heatmap handles                    #
#------------------------------------------------------------------------------#

sel_KUI <- function(id) {
  ns <- NS(id)
  #print(paste("ui", ns(id)))
  print(paste("ui", ns("sel_K")))
  
  #uiOutput(ns("sel_K"))
  
  box(
    title = "H Matrix Heatmap", 
    width = 3, 
    height = 350,
    solidHeader = TRUE, status = "primary",
    uiOutput(ns("sel_K")),
    
    prettySwitch(
      inputId = ns("hmatheat_showcol"),
      label = "Column names",
      value = TRUE,
      status = "success",
      fill = TRUE
    ), 
    
    prettySwitch(
      inputId = ns("hmatheat_cluster_rows"),
      label = "Cluster rows",
      value = TRUE,
      status = "success",
      fill = TRUE
    ),
    
    prettySwitch(
      inputId = ns("hmatheat_cluster_cols"),
      label = "Cluster columns",
      value = TRUE,
      status = "success",
      fill = TRUE
    ),
    
    prettySwitch(
      inputId = ns("hmatheat_annot"),
      label = "Show annotation",
      value = TRUE,
      status = "success",
      fill = TRUE
    ),
    
    uiOutput(ns("inputannot_selcols"))
    
  )
  
}


#------------------------------------------------------------------------------#
#                               Heatmap Server & UI                            #
#------------------------------------------------------------------------------#
HHeatmapServerUI <- function(id) {
  plotOutput(NS(id, "plot_hmatrixheat"))
}

HHeatmapServer <- function(id, nmf_obj, annot_react) {
  moduleServer(id, function(input, output, session) {
    
    # Heatmap Annot
    heat_anno <- reactiveVal(NULL)
    observeEvent({
      input$hmatheat_annot
      input$inputannot_selcols
      input$sel_K
      nmf_obj()
    }, {
      #print("Heatmap Annot .....")
      #print(paste("Annot cols: ", input$inputannot_selcols))
      #req(nmf_obj_react())
      req(nmf_obj())
      req(input$sel_K)
      #req(input$inputannot_selcols)
      k <- input$sel_K
      
      
      hmat <- HMatrix(nmf_obj(), k = k)
      
      
      if (input$hmatheat_annot & !is.null(annot_react()) & length(input$inputannot_selcols) > 0) {
        # Build Heatmap annotation
        #print("Heatmap Annot ..... update?")
        annot <- annot_react()
        annot <- annot[match(colnames(hmat), annot[,1]), -1, drop=FALSE]
        annot <- annot[, colnames(annot) %in% input$inputannot_selcols]
        
        hanno <- HeatmapAnnotation(df = annot,
                                   #col = type.colVector,
                                   show_annotation_name = FALSE, na_col = "white")
      } else {
        hanno <- NULL
      }
      
      
      heat_anno(hanno)
    })
    
    # Heatmap
    observeEvent({
      nmf_obj()
      heat_anno()
      input$sel_K
      #input$hmatheat_showcol
      #input$hmatheat_cluster_rows
      #input$hmatheat_cluster_cols
      #heat_anno()
      #input$hmatheat_annot
      #input$inputannot_selcols
    }, {
      #print("Heatmap .....")
      req(nmf_obj())
      
      output$plot_hmatrixheat <- renderPlot({
        req(nmf_obj())
        req(input$sel_K)
        k <- input$sel_K
        hmat <- HMatrix(nmf_obj(), k = k)
        
        Heatmap(hmat, 
                col = viridis(100),
                name = "Exposure",
                cluster_columns             = input$hmatheat_cluster_cols,
                clustering_distance_columns = "pearson",
                show_column_dend            = TRUE, 
                heatmap_legend_param = 
                  list(color_bar = "continuous", legend_height=unit(2, "cm")),
                #top_annotation    = heat_anno,
                top_annotation    = heat_anno(),
                show_column_names = input$hmatheat_showcol,
                cluster_rows      = input$hmatheat_cluster_rows,
                show_row_names    = FALSE)
      },
      #width  = 100, 
      height = 330
      )
      
    })
    
  })
}
