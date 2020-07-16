#------------------------------------------------------------------------------#
#                 Server Make K picker and Annot picker                        #
#------------------------------------------------------------------------------#
sel_KServer <- function(id, nmf_obj, annot_react, colsel_label, colsel_multi) {
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
          label    = colsel_label, 
          choices  = colnames(annot_react())[-1],
          selected = colnames(annot_react())[2],
          options = list(
            `actions-box` = TRUE), 
          multiple = colsel_multi
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
#           UI  K picker and Annot picker Recovery plots                       #
#------------------------------------------------------------------------------#

sel_KrecovUI <- function(id, title) {
  ns <- NS(id)
  box(
    title = title, 
    width = 3, 
    height = 350,
    solidHeader = TRUE, status = "primary",
    uiOutput(ns("sel_K")),
    uiOutput(ns("inputannot_selcols"))
  )
}



#------------------------------------------------------------------------------#
#                               Heatmap Server & UI                            #
#------------------------------------------------------------------------------#
HHeatmapUI <- function(id) {
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
        k <- input$sel_K
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

#------------------------------------------------------------------------------#
#                        UMAP H matrix Server & UI                             #
#------------------------------------------------------------------------------#
humapUI <- function(id) {
  plotOutput(NS(id, "plot_HUMAP"))
}

# humapServer <- function(id, nmf_obj, annot_react) {
#   moduleServer(id, function(input, output, session) {
#     
#     
#     # Heatmap Annot
#     umap_df <- reactiveVal(NULL)
#     observeEvent({
#       nmf_obj()
#       input$sel_K
#     }, {
#       req(nmf_obj())
#       
#       k <- input$sel_K
#       hmat <- HMatrix(nmf_obj(), k = k)
#       umapView <- umap(t(hmat))
#       
#       umapView_df <- as.data.frame(umapView$layout)
#       colnames(umapView_df) <- c("UMAP1", "UMAP2")
#       
#       annot <- annot_react() %>% 
#         mutate(sampleID = .[[1]])
#         
#       
#       umapView_df <- umapView_df %>%
#         rownames_to_column("sampleID") %>% 
#         left_join(annot, by = "sampleID")
#       
#       
#       
#       
#       #print(umapView_df)
#       umap_df(umapView_df)
#     })
#     
#     # specify the id 
#     w <- Waiter$new(id = "plot_HUMAP")
#     
#     observeEvent({
#       #nmf_obj()
#       #input$sel_K
#       umap_df()
#       input$inputannot_selcols
#     }, {
#       #req(nmf_obj())
#       
#       hheat <- reactive({
#         
#         req(nmf_obj())
#         req(input$sel_K)
#         umap_df() %>%
#           ggplot(aes(x=UMAP1, y=UMAP2, color = !! sym(input$inputannot_selcols))) + 
#           geom_point(size = 1.5, alpha = 0.95) + 
#           theme_cowplot()
#       })
#       
#       
#       output$plot_HUMAP <- renderPlot({
#         
#         # if (is.character(annot_char) | is.factor(annot_char)) {
#         #   recovery_plot(hmat, annot_char)
#         # } else {
#         #   ggplot() + 
#         #     annotate("text", x = 0, y = 0, 
#         #              label = c("Please select a categorical variable")) +
#         #     theme_void()
#         # }
#       }, height = 300
#       )
#       
#       
#       
#       # output$plot_HUMAP <- renderPlot({
#       #   req(nmf_obj())
#       #   req(input$sel_K)
#       #   umap_df() %>%
#       #     ggplot(aes(x=UMAP1, y=UMAP2, color = !! sym(input$inputannot_selcols))) + 
#       #     geom_point(size = 1.5, alpha = 0.95) + 
#       #     theme_cowplot()
#       #   # if (is.character(annot_char) | is.factor(annot_char)) {
#       #   #   recovery_plot(hmat, annot_char)
#       #   # } else {
#       #   #   ggplot() + 
#       #   #     annotate("text", x = 0, y = 0, 
#       #   #              label = c("Please select a categorical variable")) +
#       #   #     theme_void()
#       #   # }
#       # }, height = 300
#       # )
#     })
#   })
# }





humapServer <- function(id, nmf_obj, annot_react) {
  moduleServer(id, function(input, output, session) {
    
    
    # specify the id 
    # ns <- session$ns
    # w <- Waiter$new(id = ns("plot_HUMAP"), ) # use the namespace
    
    # UAMP run and return dataframe
    umap_df <- reactiveVal(NULL)
    observeEvent({
      nmf_obj()
      annot_react()
      input$sel_K
    }, {
      #w$show() # WAITER
      #Sys.sleep(5)
      
      # Run UMAP
      req(nmf_obj())
      k <- input$sel_K
      hmat <- HMatrix(nmf_obj(), k = k)
      umapView <- umap(t(hmat))
      # Make df and bind annotation
      umapView_df <- as.data.frame(umapView$layout)
      colnames(umapView_df) <- c("UMAP1", "UMAP2")
      if (!is.null(annot_react())) {
        annot <- annot_react() %>% 
          mutate(sampleID = .[[1]])
        umapView_df <- umapView_df %>%
          rownames_to_column("sampleID") %>% 
          left_join(annot, by = "sampleID")
      }
      #print(umapView_df)
      umap_df(umapView_df)
    })
    
    
    
    observeEvent({
      #nmf_obj()
      #input$sel_K
      umap_df()
      input$inputannot_selcols
    }, {
      #req(nmf_obj())
      output$plot_HUMAP <- renderPlot({
        req(nmf_obj())
        req(input$sel_K)
        
        if (length(input$inputannot_selcols) == 1 & 
            !is.null(annot_react()) &
            (input$inputannot_selcols %in% colnames(umap_df())) ) {
          ggplot(umap_df(), aes(x=UMAP1, y=UMAP2, color = !! sym(input$inputannot_selcols))) +
            geom_point(size = 1.5, alpha = 0.95) +
            theme_cowplot()
        } else {
          ggplot(umap_df(), aes(x=UMAP1, y=UMAP2)) +
            geom_point(size = 1.5, alpha = 0.95) +
            theme_cowplot()
        }
         
          
        # if (is.character(annot_char) | is.factor(annot_char)) {
        #   recovery_plot(hmat, annot_char)
        # } else {
        #   ggplot() +
        #     annotate("text", x = 0, y = 0,
        #              label = c("Please select a categorical variable")) +
        #     theme_void()
        # }
      }, height = 300
      )
    })
  })
}


#------------------------------------------------------------------------------#
#                        Recovery plots Server & UI                            #
#------------------------------------------------------------------------------#
recovplotsUI <- function(id) {
  plotOutput(NS(id, "plot_recoveryplots"))
}

recovplotsServer <- function(id, nmf_obj, annot_react) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent({
      nmf_obj()
      input$sel_K
      input$inputannot_selcols
    }, {
      #req(nmf_obj())
      output$plot_recoveryplots <- renderPlot({
        req(nmf_obj())
        req(input$sel_K)
        
        k <- input$sel_K
        hmat <- HMatrix(nmf_obj(), k = k)
        annot <- annot_react()
        # annot <- annot[match(colnames(hmat), annot[,1]), -1, drop=FALSE]
        # annot <- annot[, input$inputannot_selcols_recov]
        
        # make factor of selected annotation
        if (!input$inputannot_selcols %in% colnames(annot)) {
          annot_char <- FALSE
        } else {
          annot_char <- setNames(annot[, input$inputannot_selcols], 
                                 annot[,1])
        }
        
        
        
        if (is.character(annot_char) | is.factor(annot_char)) {
          recovery_plot(hmat, annot_char)
        } else {
          ggplot() + 
            annotate("text", x = 0, y = 0, 
                     label = c("Please select a categorical variable")) +
            theme_void()
        }
      },
      height = 300
      )
    })
  })
}













