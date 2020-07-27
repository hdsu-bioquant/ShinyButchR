#------------------------------------------------------------------------------#
#                        UMAP H matrix Server & UI                             #
#------------------------------------------------------------------------------#
humapUI <- function(id) {
  tagList(uiOutput(NS(id, "title_sel_K")),
          plotOutput(NS(id, "plot_HUMAP")))
  #plotOutput(NS(id, "plot_HUMAP"))
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
      req(input$sel_K %in% nmf_obj()@OptKStats$k)
      
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
      #input$inputannot_selcols
    }, {
      #req(nmf_obj())
      output$plot_HUMAP <- renderPlot({
        req(nmf_obj())
        req(input$sel_K)
        #print(input$inputannot_selcols) 
        # if (length(input$inputannot_selcols) == 1 & 
        #     !is.null(annot_react()) &
        #     (input$inputannot_selcols %in% colnames(umap_df())) ) {
        #   ggplot(umap_df(), aes(x=UMAP1, y=UMAP2, color = !! sym(input$inputannot_selcols))) +
        #     geom_point(size = 1.5, alpha = 0.95) +
        #     theme_cowplot()
        # }
        if (length(input$inputannot_selcols) == 1 & 
            !is.null(annot_react()) ) {
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

