library(ggplot2)
library(dplyr)
library(shinyjs)
library(ComplexHeatmap)
library(viridis)

function(input, output, session) {
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$br(),
      tags$strong(h1("Welcome to")),
      tags$br(),
      tags$strong(h1("ShinyWurst")),
      #tags$img(src="map_my_corona_logo.png",height="100px"),
      tags$br(),
      tags$br(),
      "Loading ...", 
      #tags$b("Demo"),
      "Please wait until this message closes", 
      tags$br()
    ),
    html = TRUE
  )
  
  #----------------------------------------------------------------------------#
  #                            Startup config                                  #
  #----------------------------------------------------------------------------#
  if (Sys.info()[["sysname"]] == "Darwin") {
    # When running locally use conda env
    reticulate::use_condaenv("tensor2pip", required = TRUE)
    print(reticulate::py_config())
  } else {
    
    # When running on shinyapps.io, create a virtualenv
    reticulate::virtualenv_create(envname = "pytensor_env",
                                 python = "/usr/bin/python3")
    # reticulate::use_virtualenv("pytensor_env", required = T)
    # reticulate::virtualenv_install("pytensor_env",
    #                                packages = c("numpy", "tensorflow"))
    reticulate::virtualenv_install("pytensor_env", packages = c("numpy"))
    #reticulate::virtualenv_install("pytensor_env", packages = c("tensorflow"))
    reticulate::use_virtualenv("pytensor_env", required = T)
    library("tensorflow")
    tensorflow::install_tensorflow(method = "virtualenv", envname = "pytensor_env")
    #reticulate::use_python("/usr/bin/python3")
    #print(reticulate::py_config())
    #print("tensorflow available: ")
    #print(reticulate::py_module_available("tensorflow"))
    #path <- file.path(system.file(package = "Bratwurst"), "python/")
    #print(list.files(path, recursive = TRUE))
    #tensorBratwurst <- import_from_path("tensorBratwurst", path = path)
    # library(reticulate)
    #print(tensorBratwurst$tensor_iNMF_lite)
    # print(py_list_attributes(tensorBratwurst))
    # print("imported")
    
  }
  library(reticulate)
  library(Bratwurst)
  
  #----------------------------------------------------------------------------#
  #                            Reactive Values                                 #
  #----------------------------------------------------------------------------#
  
  sendSweetAlert(
    session = session,
    title = NULL,
    text = tags$span(
      tags$br(),
      tags$strong(h1("Welcome to")),
      tags$br(),
      tags$strong(h1("ShinyWurst")),
      #tags$img(src="map_my_corona_logo.png",height="100px"),
      tags$br(),
      tags$br(),
      "Click the", 
      tags$b("Demo"),
      " button if you want to try our demo matrix", 
      tags$br()
    ),
    html = TRUE
  )
  
  # toggle(id = "nmfinput", anim = TRUE, animType = "slide",
  #             time = 1.5, selector = NULL, asis = FALSE)
  
  # Reactive input matrix
  inputMatrix_react <- reactiveVal()
  # Reactive matrix annot
  annot_react <- reactiveVal()
  # Reactive NMF results
  nmf_obj_react <- reactiveVal()
  
  ##--------------------------------------------------------------------------##
  ##                              Load demo data                              ##
  ##--------------------------------------------------------------------------##
  observeEvent({
    input$load_demo
  }, {
    
    # read matrix
    #norm_mat_list <- readRDS("data/buenrostro_AML_multiview_norm_mat_list.RDS")
    norm_mat_rnaseq <- readRDS("data/rnaseq_normalized_counts.RDS")
    #lapply(norm_mat_list, dim)
    #lapply(norm_mat_list, "[", 1:5, 1:5)
    # Read annot
    #annot <- readRDS("data/buenrostro_AML_multiview_annotation.RDS") %>% 
    annot <- readRDS("data/rnaseq_annotation.RDS") %>% 
      select(sampleID, Celltype) %>% 
      mutate(nsample = 1:n())
    #annot <- annot[,c("sampleID", "Celltype", "color")]
    
    # Reactive matrix
    #inputMatrix_react(norm_mat_list[[1]])
    inputMatrix_react(norm_mat_rnaseq)
    # Reactive matrix annot
    annot_react(annot)
    
    
    updateNumericInput(session, "params_kmin", value = 2)
    updateNumericInput(session, "params_kmax", value = 10)
    updateNumericInput(session, "params_ninits", value = 2)
    updateNumericInput(session, "params_convthrs", value = 30)
    
  })
  
  ##--------------------------------------------------------------------------##
  ##                              Upload data                                 ##
  ##--------------------------------------------------------------------------##
  observeEvent({
    input$file1
  }, {
    my_path <- input$file1$datapath
    ext <- tools::file_ext(my_path)
    print(ext)
    
    if (ext %in% c("RDS", "rds", "Rds")) {
      mymat <- readRDS(my_path)
    } else if (ext %in% c("CSV", "csv", "Csv")) {
      mymat <- as.matrix(read_csv(my_path))
    } else {
      sendSweetAlert(
        session = session,
        title = "Invalid file type",
        text = "please upload csv or RDS file",
        type = "error"
      )
      mymat <- NULL
    }
    
    if (!is.numeric(mymat) ) {
      sendSweetAlert(
        session = session,
        title = "Invalid matrix",
        text = "please upload a non negative matrix",
        type = "error"
      )
      mymat <- NULL
    } else if (min(mymat) <=0) {
      sendSweetAlert(
        session = session,
        title = "Invalid matrix",
        text = "please upload a non negative matrix",
        type = "error"
      )
      mymat <- NULL
    }
    
    
    
    
    inputMatrix_react(mymat)
  })
  
  observeEvent({
    input$file_annot
  }, {
    my_path <- input$file_annot$datapath
    ext <- tools::file_ext(my_path)
    print(ext)
    
    if (ext %in% c("RDS", "rds", "Rds")) {
      myannot <- readRDS(my_path)
    } else if (ext %in% c("CSV", "csv", "Csv")) {
      myannot <- read_csv(my_path)
    } else {
      sendSweetAlert(
        session = session,
        title = "Invalid file type",
        text = "please upload csv or RDS file",
        type = "error"
      )
      myannot <- NULL
    }
    annot_react(myannot)
  })
  
  ##--------------------------------------------------------------------------##
  ##                              Load  data aux                              ##
  ##--------------------------------------------------------------------------##
  output$inputmatrix_printdim <- renderText({
    req(inputMatrix_react())
    d <- dim(inputMatrix_react())
    paste0("Uploaded matrix with ", d[1], " rows, and ", d[2], " columns")
  })
  output$inputmatrix_printout <- renderTable({
    #print(annot_react()[1:5,])
    inputMatrix_react()[1:5,1:5]
    },
    spacing = "xs",
    rownames = TRUE
  )
  
  output$inputannot_printdim <- renderText({
    req(annot_react())
    d <- dim(annot_react())
    paste0("Uploaded annotation with ", d[1], " rows, and ", d[2], " columns")
  })
  output$inputannot_printout <- renderTable({
    annot_react()[1:5,]
  },
  spacing = "xs",
  rownames = TRUE
  )
  
  # K selector
  output$inputannot_selcols <- renderUI({
    req(annot_react())
    
    pickerInput(
      inputId  = "inputannot_selcols",
      label    = "Select columns to use", 
      choices  = colnames(annot_react())[-1],
      selected = colnames(annot_react())[2],
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    )
    
    
  })
  
  ##--------------------------------------------------------------------------##
  ##                          Clear uploaded data                             ##
  ##--------------------------------------------------------------------------##
  observeEvent(input$clear_inputMatrix, {
    #reset("stringSequence")
    reset("file1")
    reset("file_annot")
    
    #clearstatus$clear <- TRUE
    inputMatrix_react(NULL)
    annot_react(NULL)
    
    updateNumericInput(session, "params_kmin", value = 2)
    updateNumericInput(session, "params_kmax", value = 3)
    updateNumericInput(session, "params_ninits", value = 2)
    updateNumericInput(session, "params_convthrs", value = 40)
    
    
  }, priority = 1000)
  
  ##--------------------------------------------------------------------------##
  ##                                 Run NMF                                  ##
  ##--------------------------------------------------------------------------##
  #print(annot)
  observeEvent({
    #clean_start()
    #new_run()
    input$startNMF
  }, {
    
    req(inputMatrix_react())
    #print(annot)
    
    sendSweetAlert(
      session = session,
      title = "NMF in progress...",
      text = "Please wait for the results",
      type = "info"
    )
    
    
    nmf_exp <- runNMFtensor_lite(inputMatrix_react(),
                                 ranks  = input$params_kmin:input$params_kmax,
                                 method = input$params_method,
                                 n_initializations     = input$params_ninits,
                                 iterations            = 10^4,
                                 convergence_threshold = input$params_convthrs)
    nmf_exp <- normalizeW(nmf_exp)
    
    
    
    
    nmf_obj_react(nmf_exp)
    
    print("NMF results:")
    print(nmf_obj_react())
    
    sendSweetAlert(
      session = session,
      title = "NMF complete",
      text = "You can explore the results in the 'NMF plots' tab 
      and download them in the 'Save results' tab  ",
      type = "info"
    )
    
    # toggle(id = "nmfplots", anim = TRUE, animType = "slide",
    #        time = 1.5, selector = NULL, asis = FALSE)
    #updateTabItems(session, "nmfplots", newtab)
    updateTabsetPanel(session, "mainmenu", "nmfplots")
    #show(id = "nmfplots")
    
  })
  
  ##--------------------------------------------------------------------------##
  ##                                 NMF Heatmap                              ##
  ##--------------------------------------------------------------------------##
  # K selector
  output$sel_K <- renderUI({
    req(nmf_obj_react())
    ks <- nmf_obj_react()@OptKStats$k
    optk <- nmf_obj_react()@OptK
    print(optk)
    print(length(optk))
    selectInput(
      inputId = "sel_K",
      #label = "Select factorization rank:",
      label = "Select K:",
      choices = ks,
      selected = ifelse(length(optk) == 0,
                         ks[1], max(optk)),
      multiple = FALSE
    )
  })
  
  # Heatmap Annot
  heat_anno <- reactiveVal(NULL)
  observeEvent({
    input$hmatheat_annot
    input$inputannot_selcols
    input$sel_K
    nmf_obj_react()
  }, {
    print("Heatmap Annot .....")
    #req(nmf_obj_react())
    req(nmf_obj_react())
    req(input$sel_K)
    #req(input$inputannot_selcols)
    
    k <- input$sel_K
    hmat <- HMatrix(nmf_obj_react(), k = k)
    
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
    nmf_obj_react()
    input$sel_K
    input$hmatheat_showcol
    input$hmatheat_cluster_rows
    input$hmatheat_cluster_cols
    #heat_anno()
    #input$hmatheat_annot
    #input$inputannot_selcols
  }, {
    print("Heatmap .....")
    req(nmf_obj_react())
    
    output$plot_hmatrixheat <- renderPlot({
      req(nmf_obj_react())
      req(input$sel_K)
      #req(input$inputannot_selcols)
      
      k <- input$sel_K
      hmat <- HMatrix(nmf_obj_react(), k = k)
      #print(hmat)
      
      # if (input$hmatheat_annot & !is.null(annot_react()) & length(input$inputannot_selcols) > 0) {
      #   
      #   # Build Heatmap annotation
      #   # heat_anno <- HeatmapAnnotation(df = data.frame(Celltype = annot_react()$Celltype),
      #   #                                #col = type.colVector,
      #   #                                show_annotation_name = FALSE, na_col = "white")
      #   annot <- annot_react()
      #   annot <- annot[match(colnames(hmat), annot[,1]), -1, drop=FALSE]
      #   
      #   
      #   annot <- annot[, colnames(annot) %in% input$inputannot_selcols]
      #   
      #   heat_anno <- HeatmapAnnotation(df = annot,
      #                                  #col = type.colVector,
      #                                  show_annotation_name = FALSE, na_col = "white")
      #   
      # } else {
      #   heat_anno <- NULL
      # }
      
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
  
  ##--------------------------------------------------------------------------##
  ##                                 NMF Riverplot                            ##
  ##--------------------------------------------------------------------------##
  output$sel_riverRange <- renderUI({
    req(nmf_obj_react())
    ks <- nmf_obj_react()@OptKStats$k
    sliderTextInput(
      inputId = "sel_riverRange",
      label = "Select river plot range:", 
      choices = ks,
      selected = c(min(ks), max(ks)),
      grid = TRUE
    )
  })
  
  
  observeEvent({
    nmf_obj_react()
    input$sel_riverRange
    input$sel_edges_cutoff
  }, {
    req(nmf_obj_react())
    req(input$sel_riverRange)
    
    output$plot_riverplot <- renderPlot({
      req(nmf_obj_react())
      ranks <- input$sel_riverRange
      ranks <- ranks[1]:ranks[2]
      # (coords <- plot(Bratwurst::generateRiverplot(nmf_obj_react()),
      #                 plot_area = 1, autoy=FALSE))
      log <- capture.output({
        res <- plot(generateRiverplot(nmf_obj_react(),
                                      edges.cutoff = input$sel_edges_cutoff,
                                      ranks = ranks),
                    plot_area = 1, autoy=FALSE);
      })
      #print(res);
      
    },
    height = 400
    )
  })
  
  
  ##--------------------------------------------------------------------------##
  ##                                 NMF OptK                                 ##
  ##--------------------------------------------------------------------------##
  output$print_optK <- renderUI({
    req(nmf_obj_react())
    
    
    optk <- nmf_obj_react()@OptK
    if (length(optk) == 0) {
      h5("could not be found", align = "center")
    } else {
      h5(paste0("was k = ", paste(optk, collapse = ",")), align = "center")
    }
    
  })
  
  output$plot_nmfoptk <- renderPlot({
    req(nmf_obj_react())
    gg_plotKStats(nmf_obj_react())
    
  },
  height = 300
  )
  
  
  
  
  ##--------------------------------------------------------------------------##
  ##                             Download Handlers                            ##
  ##--------------------------------------------------------------------------##
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadNMFobject <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "NMF_object.RDS"
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      saveRDS(nmf_obj_react(), file)
      
    }
  )
  
  
  # K selector
  output$download_sel_K <- renderUI({
    req(nmf_obj_react())
    ks <- nmf_obj_react()@OptKStats$k
    optk <- nmf_obj_react()@OptK
    print(optk)
    print(length(optk))
    selectInput(
      inputId = "download_sel_K",
      #label = "Select factorization rank:",
      label = "Select K:",
      choices = ks,
      selected = ifelse(length(optk) == 0,
                        ks[1], max(optk)),
      multiple = FALSE
    )
  })
  
  output$downloadHmatrix <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("NMF_", input$download_matrixwhich, 
             "_matrix_K", input$download_sel_K,
             "_", input$download_matrixformat)
      
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      k <- input$download_sel_K
      if (input$download_matrixwhich == "H") {
        downmat <- HMatrix(nmf_obj_react(), k = k)
        rownames(downmat) <- paste0("signature",  1:k)
      } else {
        downmat <- WMatrix(nmf_obj_react(), k = k)
        colnames(downmat) <- paste0("signature",  1:k)
      }
      
      # Write to a file specified by the 'file' argument
      if (input$download_matrixformat == ".RDS") {
        saveRDS(downmat, file)
      } else {
        write.csv(x = downmat, file = file, row.names = TRUE, quote = FALSE)
      }
      
      
    }
  )
  
  
  
  
  
}
