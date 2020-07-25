#------------------------------------------------------------------------------#
#                 Server Make K picker and Annot picker                        #
#------------------------------------------------------------------------------#


sel_KServer <- function(id, nmf_obj, annot_react, colsel_label, colsel_multi, newRun) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$sel_K, {
        output$title_sel_K <- renderUI({
          #nmf_obj()
          
          absolutePanel(h1(paste0("k=", input$sel_K)), 
                     top = "280px",
                     left = NULL,
                     right = "50px",
                     bottom = NULL,
                     width = 10,
                     height = 10,
                     draggable = TRUE)
          
          # HTML(paste0("<p style='text-align:left'>",
          #             "This text is left aligned",
          #             "<span style='float:right;'>",
          #             input$sel_K,
          #             #"This text is right aligned",
          #             "</span>
          #             </p>"))


          #tags$p(paste0("blablabla ", input$sel_K))


        })
      }, priority = 900)
      
      # Select K
      observeEvent(nmf_obj(), {
        output$sel_K <- renderUI({
          #nmf_obj()
          req(nmf_obj())
          
          
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
          
          # if (is.null(nmf_obj())) {
          #   
          #   selectInput(
          #     inputId = ns("sel_K"),
          #     #label = "Select factorization rank:",
          #     label = "Select K:",
          #     choices = NA,
          #     selected = NA,
          #     multiple = FALSE
          #   )
          #   
          # } else {
          #   ks <- nmf_obj()@OptKStats$k
          #   optk <- nmf_obj()@OptK
          #   
          #   selectInput(
          #     inputId = ns("sel_K"),
          #     #label = "Select factorization rank:",
          #     label = "Select K:",
          #     choices = ks,
          #     selected = ifelse(length(optk) == 0,
          #                       ks[1], max(optk)),
          #     multiple = FALSE
          #   )
          # }
          
          
          
        })
      }, priority = 1000)
      
      
      
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
# sel_KServer <- function(id, nmf_obj, annot_react, colsel_label, colsel_multi, newRun) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#       #ns <- session$ns
#       
#       observeEvent(newRun(), {
#         print(paste("run", newRun(), "null to ", id))
#         ns <- NS(id)
#         # 
#         updateSelectInput(session, ns("sel_K"), selected = 1)
#         updateSelectInput(session, ns("inputannot_selcols"))
#         # updateSelectInput(session, "sel_K", selected = 1)
#         # updateSelectInput(session, "inputannot_selcols")
#         #heat_anno <- reactiveVal(NULL)
#         print(input$sel_K)
#         ##input$hmatheat_annot
#         #input$inputannot_selcols
#         #input$sel_K
#         
#       })
#       
#       
#       # Select K
#       output$sel_K <- renderUI({
#         #nmf_obj()
#         #req(nmf_obj())
#         
#         #ns <- session$ns
#         ns <- NS(id)
#         ks <- nmf_obj()@OptKStats$k
#         optk <- nmf_obj()@OptK
#         
#         selectInput(
#           inputId = ns("sel_K"),
#           #label = "Select factorization rank:",
#           label = "Select K:",
#           choices = ks,
#           selected = ifelse(length(optk) == 0,
#                             ks[1], max(optk)),
#           multiple = FALSE
#         )
#       })
#       
#       
#       # Selector columns to use
#       output$inputannot_selcols <- renderUI({
#         req(annot_react())
#         ns <- NS(id)
#         pickerInput(
#           inputId  = ns("inputannot_selcols"),
#           label    = colsel_label, 
#           choices  = colnames(annot_react())[-1],
#           selected = colnames(annot_react())[2],
#           options = list(
#             `actions-box` = TRUE), 
#           multiple = colsel_multi
#         )
#       })
# 
#     }
#   )
# }

#------------------------------------------------------------------------------#
#           UI  K picker and Annot picker & Heatmap handles                    #
#------------------------------------------------------------------------------#

sel_KUI <- function(id) {
  ns <- NS(id)
  box(
    #title = uiOutput(ns("title_sel_K")),
    title = "H Matrix Heatmap", 
    # title = tagList("H Matrix Heatmap",
    #                 uiOutput(ns("title_sel_K"))), 
    
    # title = tagList(#"<p>H Matrix Heatmap</p>",
    #                 tags$p("H Matrix Heatmap"),
    #                 uiOutput(ns("title_sel_K"))),
    
    # <p style="text-align:left;">
    #   This text is left aligned
    # <span style="float:right;">
    #   This text is right aligned
    # </span>
    #   </p>
    
    
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


