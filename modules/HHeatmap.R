
#------------------------------------------------------------------------------#
#                               Heatmap Server & UI                            #
#------------------------------------------------------------------------------#

HHeatmapUI <- function(id) {
  tagList(uiOutput(NS(id, "title_sel_K")),
          plotOutput(NS(id, "plot_hmatrixheat")))
  #plotOutput(NS(id, "plot_hmatrixheat"))
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
      req(input$sel_K %in% nmf_obj()@OptKStats$k)
      
      #req(input$inputannot_selcols)
      k <- as.numeric(input$sel_K)
      #print(paste(class(k), k))
      
      
      hmat <- HMatrix(nmf_obj(), k = k)
      
      
      if (input$hmatheat_annot & !is.null(annot_react()) & length(input$inputannot_selcols) > 0) {
        # Build Heatmap annotation
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
    }, {
      #print("Heatmap .....")
      req(nmf_obj())
      output$plot_hmatrixheat <- renderPlot({
        req(nmf_obj())
        req(input$sel_K)
        req(input$sel_K %in% nmf_obj()@OptKStats$k)
        
        
        k <- as.numeric(input$sel_K)
        hmat <- HMatrix(nmf_obj(), k = k)
        
        Heatmap(hmat, 
                col = viridis(100),
                name = "Exposure",
                cluster_columns             = input$hmatheat_cluster_cols,
                clustering_distance_columns = "pearson",
                show_column_dend            = TRUE, 
                # heatmap_legend_param = 
                #   list(color_bar = "continuous", legend_height=unit(2, "cm")),
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


