library(matrixStats)
#------------------------------------------------------------------------------#
#           UI  K picker and Annot picker & Heatmap handles                    #
#------------------------------------------------------------------------------#

feature_extractionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3, 
        box(
          title = "Signature Specific Features Heatmap", 
          width = NULL,
          #width = 3, 
          height = 370,
          solidHeader = TRUE, status = "primary",
          uiOutput(ns("sel_K")),
          
          sliderTextInput(
            inputId = ns("topnFeatures"),
            label = "Top % of associated features per signature:", 
            choices = seq(0.5, 10, by = 0.5),
            selected = 1.0,
            grid = TRUE
          ),
          
          prettySwitch(
            inputId = ns("wmatheat_showcol"),
            label = "Column names",
            value = TRUE,
            status = "success",
            fill = TRUE
          ), 
          
          prettySwitch(
            inputId = ns("wmatheat_cluster_rows"),
            label = "Cluster rows",
            value = TRUE,
            status = "success",
            fill = TRUE
          ),
          
          prettySwitch(
            inputId = ns("wmatheat_cluster_cols"),
            label = "Cluster columns",
            value = TRUE,
            status = "success",
            fill = TRUE
          )
        ),
        box(
          title = "Selected Signature Specific Features Heatmap", 
          width = NULL,
          height = 350,
          solidHeader = TRUE, status = "primary",
          uiOutput(NS(id, "title_sel_K"))
        )
      ),
      column(
        width = 9, 
        box(
          #width = 9, 
          width = NULL,
          height = 370,
          solidHeader = TRUE,
          #HHeatmapUI("WHeat")
          #plotOutput(NS(id, "plot_main_heatmap"))
          #plotOutput(NS(id, "plot_main_heatmap"))
          plotOutput(NS(id, "plot_main_heatmap"), 
                     brush = NS(id, "ht_brush_tc"))
        ),
        box(
          #width = 9, 
          width = NULL,
          height = 350,
          solidHeader = TRUE,
          #HHeatmapUI("WHeat")
          plotOutput(NS(id, "sub_heatmap"))
        )
      )
    ),
    fluidRow(
      box(
        title = "Download Signature Specific Features table", 
        width = 12, 
        height = 50,
        solidHeader = TRUE, status = "primary",
        downloadButton('downloadTop_features', 'Download')
        
        )
    )
  )
}






#------------------------------------------------------------------------------#
#                               Heatmap Server & UI                            #
#------------------------------------------------------------------------------#

top_featuresServer <- function(id, nmf_obj) {
  moduleServer(id, function(input, output, session) {
    
    nmf_features <- reactiveVal(NULL)
    observeEvent({
      nmf_obj()
      input$sel_K
      input$topnFeatures
    }, {
      #print("Heatmap .....")
      req(nmf_obj())
      req(input$sel_K)
      req(input$sel_K %in% nmf_obj()@OptKStats$k)
      
      
      k <- as.numeric(input$sel_K)
      wmatrix <- WMatrix(nmf_obj(), k = k)
      colnames(wmatrix) <- paste0("Sign",1:ncol(wmatrix) )
      if (is.null(rownames(wmatrix))) {
        rownames(wmatrix) <- paste0("f",1:nrow(wmatrix) )
      }
      
      
      # Select only the top features according to slider
      ntop <- 1-(input$topnFeatures/100)
      #print(paste0("the quantile is ", ntop))
      
      # Top features
      selec_wmatrix <- do.call(cbind, lapply(as.data.frame(wmatrix), function(sign_expo){
        sign_expo[sign_expo < quantile(sign_expo, ntop)] <- NA
        sign_expo
      }))
      rownames(selec_wmatrix) <- rownames(wmatrix)
      nmf_feat <- lapply(setNames(colnames(wmatrix), colnames(wmatrix)), function(sigID){
        selec_wmatrix <- selec_wmatrix[!is.na(selec_wmatrix[,sigID]),,drop=FALSE]
        # Keep only the top feature if there's an overlap
        idx <- rowMaxs(selec_wmatrix, na.rm = TRUE) == selec_wmatrix[,sigID]
        rownames(selec_wmatrix[idx,,drop=FALSE])
      })
      
      wmatrix <- wmatrix[unique(do.call(c, nmf_feat)),,drop=FALSE]
      
      
      nmf_features(list(wmatrix  = wmatrix, 
                        features = nmf_feat))
      
      
    })
    
    
    
    # 
    # main_heatmap <-  renderPlot({
    #   env_i$Hmat <-  ComplexHeatmap::draw(Hmat)
    #   env_i$ht_pos <-  ht_pos_on_device(env_i$Hmat)
    # })
    
    main_heatmap_reac <- reactiveVal(NULL)
    ht_pos_reac <- reactiveVal(NULL)
    output$plot_main_heatmap <- renderPlot({
      req(nmf_features())
      req(input$sel_K)
      req(input$sel_K %in% nmf_obj()@OptKStats$k)
      
      main_heatmap <- Heatmap(nmf_features()$wmatrix, 
                              col = inferno(100),
                              name = "Exposure",
                              cluster_columns   = input$wmatheat_cluster_cols,
                              show_column_dend  = TRUE, 
                              show_column_names = input$wmatheat_showcol,
                              cluster_rows      = input$wmatheat_cluster_rows,
                              show_row_names    = FALSE)
      
      main_heatmap_reac(ComplexHeatmap::draw(main_heatmap))
      ht_pos_reac(ht_pos_on_device(main_heatmap_reac()))
      
    },
    height = 350
    )
    
    
    sub_heatmap_reac <- reactiveVal(NULL)
    output$sub_heatmap <- renderPlot({
      #print(brush)
      if(is.null(input$ht_brush_tc)) {
        #if(is.null(input$ht_brush)) {
        grid.newpage()
        grid.text("No region is selected.", 0.5, 0.5)
      } else {
        #lt = ComplexHeatmap:::get_pos_from_brush(input[[brush]])
        lt <- ComplexHeatmap:::get_pos_from_brush(input$ht_brush_tc)
        pos1 <- lt[[1]]
        pos2 <- lt[[2]]
        
        ht_matrix <- main_heatmap_reac()
        pos = selectArea(ht_matrix, mark = FALSE, pos1 = pos1, pos2 = pos2, 
                         verbose = FALSE, ht_pos = ht_pos_reac())
        
        row_index = unlist(pos[1, "row_index"])
        column_index = unlist(pos[1, "column_index"])
        m = ht_matrix@ht_list[[1]]@matrix
        ht_select = Heatmap(m[row_index, column_index, drop = FALSE],
                            col = ht_matrix@ht_list[[1]]@matrix_color_mapping@col_fun,
                            
                            show_heatmap_legend = FALSE,
                            cluster_rows = FALSE, 
                            cluster_columns = FALSE)
        sub_heatmap_reac(ComplexHeatmap::draw(ht_select))
      }
    },
    height = 330
    )
    
    
    
    
    
    output$downloadTop_features <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        "downloadTop_features.RDS"
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        req(nmf_features()$features)
        
        nmf_featuresIDs <- unique(do.call(c, nmf_features()$features))
        nmf_features_table <- do.call(cbind, lapply(nmf_features()$features, function(sigfeatIDS){
          nmf_featuresIDs %in% sigfeatIDS
        }))
        
        
        
        # sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
        
        # Write to a file specified by the 'file' argument
        saveRDS(nmf_features_table, file)
        
        
      }
    )
    
    
    
  })
}






feature_extractionServer <- function(id, nmf_obj) {
  moduleServer(id, function(input, output, session) {
    
    nmf_obj_feat <- reactiveVal(NULL)
    observeEvent(nmf_obj(), {
      #print("Heatmap .....")
      req(nmf_obj())
      
      ks <- nmf_obj()@OptKStats$k
      
      if (length(ks) == 1 & 2 %in% ks ) {
        #nmf_obj_feat("Feature extraction not supported for rank = 2")
        nmf_obj_feat(FALSE)
      } else {
        nmf_obj_feat(compute_SignatureFeatures(nmf_obj()))
      }
      
      
    })
    
    
    
    # 
    # main_heatmap <-  renderPlot({
    #   env_i$Hmat <-  ComplexHeatmap::draw(Hmat)
    #   env_i$ht_pos <-  ht_pos_on_device(env_i$Hmat)
    # })
    
    output$plot_main_heatmap <- renderPlot({
      req(nmf_obj_feat())
      req(input$sel_K)
      req(input$sel_K %in% nmf_obj_feat()@OptKStats$k)
      
      
      k <- as.numeric(input$sel_K)
      wmat <- WMatrix(nmf_obj_feat(), k = k)
      colnames(wmat) <- paste0("Sign",1:ncol(wmat) )
      wmat <- wmat[1:100,]
      
      
      Heatmap(wmat, 
              col = viridis(100),
              name = "Exposure",
              cluster_columns             = input$wmatheat_cluster_cols,
              #clustering_distance_columns = "pearson",
              show_column_dend            = TRUE, 
              # heatmap_legend_param = 
              #   list(color_bar = "continuous", legend_height=unit(2, "cm")),
              show_column_names = input$wmatheat_showcol,
              cluster_rows      = input$wmatheat_cluster_rows,
              show_row_names    = FALSE)
    },
    #width  = 100, 
    height = 300
    )
    
    
    # Heatmap
    # observeEvent({
    #   nmf_obj()
    #   input$sel_K
    # }, {
    #   #print("Heatmap .....")
    #   req(nmf_obj())
    #   output$plot_hmatrixheat <- renderPlot({
    #     req(nmf_obj())
    #     req(input$sel_K)
    #     req(input$sel_K %in% nmf_obj()@OptKStats$k)
    #     
    #     
    #     k <- as.numeric(input$sel_K)
    #     hmat <- HMatrix(nmf_obj(), k = k)
    #     
    #     Heatmap(hmat, 
    #             col = viridis(100),
    #             name = "Exposure",
    #             cluster_columns             = input$hmatheat_cluster_cols,
    #             clustering_distance_columns = "pearson",
    #             show_column_dend            = TRUE, 
    #             # heatmap_legend_param = 
    #             #   list(color_bar = "continuous", legend_height=unit(2, "cm")),
    #             top_annotation    = heat_anno(),
    #             show_column_names = input$hmatheat_showcol,
    #             cluster_rows      = input$hmatheat_cluster_rows,
    #             show_row_names    = FALSE)
    #   },
    #   #width  = 100, 
    #   height = 330
    #   )
    # })
  })
}


