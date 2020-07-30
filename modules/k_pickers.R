#------------------------------------------------------------------------------#
#                 Server Make K picker and Annot picker                        #
#------------------------------------------------------------------------------#


sel_KServer <- function(id, nmf_obj, annot_react, colsel_label, colsel_multi, newRun) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #outputOptions(output, NS(id)("sel_k"), priority = 1000)
      
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
        })
      }, priority = 900)
      
      # Select K
      # observeEvent(nmf_obj(), {
      #   
      # }, priority = 1000)
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
      #outputOptions(session$output, "sel_K", priority = 1000, suspendWhenHidden = FALSE)
      
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


