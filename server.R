library(ggplot2)
library(dplyr)
library(shinyjs)
library(ComplexHeatmap)
library(viridis)

function(input, output, session) {
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
    norm_mat_list <- readRDS("data/buenrostro_AML_multiview_norm_mat_list.RDS")
    #lapply(norm_mat_list, dim)
    #lapply(norm_mat_list, "[", 1:5, 1:5)
    # Read annot
    annot <- readRDS("data/buenrostro_AML_multiview_annotation.RDS")
    annot <- annot[,c("sampleID", "Celltype", "color")]
    
    # Reactive matrix
    inputMatrix_react(norm_mat_list[[1]])
    # Reactive matrix annot
    annot_react(annot)
    
    
    updateNumericInput(session, "params_kmin", value = 2)
    updateNumericInput(session, "params_kmax", value = 10)
    updateNumericInput(session, "params_ninits", value = 2)
    updateNumericInput(session, "params_convthrs", value = 30)
    
  })
  
  
  output$inputmatrix_printout <- renderTable({
    print(inputMatrix_react()[1:5,1:5])
    inputMatrix_react()[1:5,1:5]
    },
    spacing = "xs",
    rownames = TRUE
  )
  
  
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
      label = "Select factorization rank:",
      choices = ks,
      selected = ifelse(length(optk) == 0,
                         ks[1], max(optk)),
      multiple = FALSE
    )
  })
  
  # Heatmap
  observeEvent({
    nmf_obj_react()
    input$sel_K
    input$hmatheat_showcol
    input$hmatheat_cluster_rows
    input$hmatheat_cluster_cols
    input$hmatheat_annot
  }, {
    req(nmf_obj_react())
    
    output$plot_hmatrixheat <- renderPlot({
      req(nmf_obj_react())
      req(input$sel_K)
      k <- input$sel_K
      hmat <- HMatrix(nmf_obj_react(), k = k)
      #print(hmat)
      
      if (input$hmatheat_annot) {
        
        # Build Heatmap annotation
        # heat_anno <- HeatmapAnnotation(df = data.frame(Celltype = annot_react()$Celltype),
        #                                #col = type.colVector,
        #                                show_annotation_name = FALSE, na_col = "white")
        annot <- annot_react()
        annot <- annot[match(colnames(hmat), annot[,1]), -1, drop=FALSE]
        heat_anno <- HeatmapAnnotation(df = annot,
                                       #col = type.colVector,
                                       show_annotation_name = FALSE, na_col = "white")
        
      } else {
        heat_anno <- NULL
      }
      
      
      Heatmap(hmat, 
              col = viridis(100),
              name = "Exposure",
              cluster_columns             = input$hmatheat_cluster_cols,
              clustering_distance_columns = "pearson",
              show_column_dend            = TRUE, 
              heatmap_legend_param = 
                list(color_bar = "continuous", legend_height=unit(2, "cm")),
              top_annotation    = heat_anno,
              show_column_names = input$hmatheat_showcol,
              cluster_rows      = input$hmatheat_cluster_rows,
              show_row_names    = FALSE)
      
      
    },
    #width  = 100, 
    height = 300
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
  
  
  
  
  
  
  
  
  
  
  
  # save raw BLAST results
  blaster_react <- reactiveVal()
  # Format BLAST results and create col palette
  blaster_form_react <- reactiveValues(df = NULL)
  # Reactive tibble to store filtered BLAST results
  blaster_filt <- reactiveVal()
  
  # Filter thresholds
  filscores_reac <- reactiveValues(blastrf_pident = NULL,
                                   blastrf_evalue = NULL,
                                   blastrf_bitscore = NULL)
  
  
  
  
  #----------------------------------------------------------------------------#
  #                          clean status and new runs                         #
  #----------------------------------------------------------------------------#
  clearstatus <- reactiveValues(loaded = TRUE, clear = TRUE)
  observeEvent(input$file1, {
    clearstatus$clear <- FALSE
  }, priority = 1000)
  
  observeEvent(input$clear_stringSequence, {
    reset("stringSequence")
    reset("file1")
    clearstatus$clear <- TRUE
    
  }, priority = 1000)
  
  
  # Clean start
  new_run <- reactiveVal()
  observeEvent({
    input$searchSequence
  }, {
    blaster_react(NULL)
    blaster_filt(NULL)
    print(paste(Sys.time(), "Clean up"))
    
    filscores_reac$pident <- NULL
    filscores_reac$evalue <- NULL
    filscores_reac$bitscore <- NULL
    
    # Start with clean map
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(42, 16, 2) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter)
    })
    # leafletProxy("map") %>%
    #   clearControls() %>%
    #   clearShapes() %>%
    #   clearMarkerClusters() %>%
    #   clearMarkers()
    
    new_run(TRUE)
  }, priority = 10000)
  
  
  # clean_start <- eventReactive(input$searchSequence, {
  #   blaster_filt(NULL)
  #   print("Clean up")
  #   # Start with clean map
  #   leafletProxy("map") %>%
  #     clearControls() %>%
  #     clearShapes() %>%
  #     clearMarkerClusters() %>%
  #     clearMarkers()
  # })
  
  #----------------------------------------------------------------------------#
  #                              New run = BLAST                               #
  #----------------------------------------------------------------------------#
  # blaster_react <- eventReactive(clean_start(), {
  #   clean_start()$map
  #   #print(clean_start())
  observeEvent({
    #clean_start()
    #new_run()
    input$searchSequence
  }, {
    demoseq <- FALSE
    #blaster_filt(NULL)
    new_run(NULL)
    # if true will run demo or text from box
    if( clearstatus$loaded & clearstatus$clear ){
      #-----------------------------------------------------------------#
      #             Run from fasta sequence in box                      #
      #-----------------------------------------------------------------#
      
      if (input$stringSequence != "") {
        
        my_path <- rand_fasta_name(1)
        writeLines(input$stringSequence, my_path)
        fast_val <- validate_fasta(my_path, input$seq_type)
        reset("stringSequence")
        
        if (!fast_val) {
          blaster_filt(NULL)
          
          sendSweetAlert(
            session = session,
            title = "Invalid sequence",
            text = "please check if the sequence contains header and is of the correct type",
            type = "error"
          )
          blaster_react(NULL)
          blaster_form_react$df <- NULL
          return(NULL)
        }
        
      } else {
        #-----------------------------------------------------------------#
        #                   Run from examples                             #
        #-----------------------------------------------------------------#
        demoseq <- TRUE
        sendSweetAlert(
          closeOnClickOutside = TRUE,
          showCloseButton = FALSE, 
          session = session,
          title = "Using default fasta sequence",
          text = "MT188340 results for nucleotide search or QHN73805 for protein search",
          type = "info"
        )
        
        if (input$seq_type == "nucleotide") {
          my_path <- "testquery/SARScov2_query_nucleotide.fasta"
        } else {
          my_path <- "testquery/SARScov2_query_protein.fasta"
        }
      }
      #-----------------------------------------------------------------#
      #                Run from uploaded fasta                          #
      #-----------------------------------------------------------------#
    } else { # will use fasta file
      clearstatus$clear <- TRUE
      clearstatus$loaded <- TRUE
      reset("file1")
      
      if (input$stringSequence != "") {
        shiny::showNotification("Text box is not empty, but will use uploaded fasta file",
                                duration = 5, closeButton = TRUE,
                                type = "error")
      } 
      
      my_path <- input$file1$datapath
      
      if (input$seq_type == "nucleotide") {
        fast_val <- validate_fasta(my_path, "nucleotide")
      } else {
        fast_val <- validate_fasta(my_path, "protein")
      }
      if (!fast_val) {
        blaster_filt(NULL)
        sendSweetAlert(
          session = session,
          title = "Invalid sequence",
          text = "please check if the sequence contains header and is of the correct type",
          type = "error"
        )
        blaster_react(NULL)
        blaster_form_react$df <- NULL
        return(NULL)
      }
      
      
      
    }
    reset("file1")
    reset("stringSequence")
    #shinyjs::hide(selector = "ul.menu-open", anim = TRUE, time = 1.5)
    hide(id = "searchseq", anim = TRUE, animType = "slide",
           time = 1.5, selector = NULL, asis = FALSE)
    toggle(id = "viz_options", anim = TRUE, animType = "slide",
           time = 1.5, selector = NULL, asis = FALSE)
    #shinyjs::hide(selector = "ul.menu-open")
    
    #--------------------------------------------------------------------------#
    #                               Run BLAST                                  #
    #--------------------------------------------------------------------------#
    if (!demoseq) {
      shinyWidgets:::toastSweetAlert(
        session = session,
        position = "center",
        #animation = FALSE,
        timer = 3000,
        title = "Blasting the sequence...",
        text = "Please wait while we blast your sequence to the database",
        type = "info"
      )
    }
    
    
    # sendSweetAlert(
    #   session = session,
    #   title = "Blasting the sequence...",
    #   text = "Please wait while we blast your seqeunce to the database",
    #   type = "info"
    # )
    
    # Blast optioms
    blast_stro <- paste0("-max_target_seqs ", input$blast_nres, 
                         " -evalue ", input$blast_evalt,
                         " -outfmt 6 -num_threads 1 ")
    
    if (Sys.info()["sysname"] == "Darwin") {
      if (input$seq_type == "nucleotide") {
        # Blast command
        blast_strn <- paste0("bin/binm/blastn -query ", my_path, " -task megablast -db db/nucleotide/covid19 ")
        # Run Blast
        align <- system(paste0(blast_strn, blast_stro), intern = TRUE)
        anno_query <- read.csv("data/SARScov2_nucleotide_metadata.csv",
                               header=TRUE, stringsAsFactors = FALSE)
      } else {
        # Blast command
        blast_strp <- paste0("bin/binm/blastp -query ", my_path, " -task blastp -db db/protein/covid19 ")
        # Run Blast
        align <- system(paste0(blast_strp, blast_stro), intern = TRUE)
        anno_query <- read.csv("data/SARScov2_protein_metadata.csv",
                               header=TRUE, stringsAsFactors = FALSE)
      }
    } else {
      if (input$seq_type == "nucleotide") {
        # Blast command
        blast_strn <- paste0("bin/blastn -query ", my_path, " -task megablast -db db/nucleotide/covid19 ")
        # Run Blast
        align <- system(paste0(blast_strn, blast_stro), intern = TRUE)
        anno_query <- read.csv("data/SARScov2_nucleotide_metadata.csv",
                               header=TRUE, stringsAsFactors = FALSE)
      } else {
        # Blast command
        blast_strp <- paste0("bin/blastp -query ", my_path, " -task blastp -db db/protein/covid19 ")
        # Run Blast
        align <- system(paste0(blast_strp, blast_stro), intern = TRUE)
        anno_query <- read.csv("data/SARScov2_protein_metadata.csv",
                               header=TRUE, stringsAsFactors = FALSE)
      }
    }
    
    
    #--------------------------------------------------------------------------#
    #                               BLAST results                              #
    #--------------------------------------------------------------------------#
    #print(blaster_react())
    # Check if results are empty
    if (length(align) == 0) {
      sendSweetAlert(
        session = session,
        title = "No hits found",
        text = "Please check if sequence is of the correct type and Blast options",
        type = "error"
      )
      blaster_react(NULL)
      blaster_form_react$df <- NULL
      return(NULL)
    } 
    #print(blaster_react())
    # Create table from alignmet result, merge with metada and sort
    align <- do.call("rbind", strsplit(align, "\t"))
    colnames(align) <- c("qaccver", "Accession", "pident", "length", 
                         "mismatch", "gapopen", "qstart", "qend", "sstart", 
                         "send", "evalue", "bitscore")
    align <- as_tibble(align) %>% 
      mutate_at(c("pident", "length", "mismatch", "gapopen", "qstart", "qend", 
                  "sstart", "send", "evalue", "bitscore"), as.numeric) %>% 
      left_join(anno_query, by = "Accession") %>% 
      arrange(desc(pident), evalue, desc(bitscore)) %>% 
      filter(!Geo_Location == "") %>% 
      filter(pident >= input$blast_pident_range[1] & pident <= input$blast_pident_range[2]) %>% 
      mutate(collection_months = substr(Collection_Date, 1, 7)) %>% 
      mutate(evalue_log10 = -log10(evalue + 1e-13))
    
    # Update blaster_react
    blaster_react(align)
    
    print(paste(Sys.time(), "BLAST done"))
    
  }, priority = 100)
  
  #-----------------------------------------------------------------#
  #         Update blaster_form_react =  format BLAST result        #
  #-----------------------------------------------------------------#
  #blaster_form_react <- reactiveValues(df = NULL)
  observeEvent({
    input$score_id
    input$sel_area_col
    input$specialclust_switch
    blaster_react()
  }, {
    print(paste(Sys.time(), "new layout"))
    req(blaster_react())
    
    score_id <- names(score_ids)[score_ids %in% input$score_id]
    
    x <- blaster_react() %>% 
      filter(!Geo_Location == "") %>% 
      #mutate(x = !! sym(score_id)) %>%  
      mutate(radius = cut(!! sym(score_id), 4)) 
    radius_levels <- setNames(seq(2, 8, 2), levels(x$radius))
    
    
    #radius_levels <- setNames(seq(3, 12, 3), levels(blaster$radius))
    x <- x %>% 
      mutate(radius = recode(radius, !!!radius_levels)) %>% 
      group_by(Geo_Location) %>% 
      mutate(idx_location = 1:n()) %>% 
      mutate(radiusfix = if_else(idx_location == 1 & sum(idx_location) > 1, 12, radius )) %>% 
      ungroup() %>% 
      mutate(radius = factor(radius, levels = sort(unique(radius)))) %>% 
      mutate(radiusfix = factor(radiusfix, levels = sort(unique(radiusfix))))
    
    #print(x$radius)
    
    dots_pal <- colorFactor(c("grey20", "grey40", "grey60", "Tomato"), domain = levels(x$radius))
    #------------------------------------#
    #         Country mapper to sp       #
    #------------------------------------#
    #countries
    simp_id <- function(x){
      gsub(" ", "", tolower(x))
    }
    
    # Country mapper
    country_mapper <- data.frame(blast_id_orig = unique(x$Geo_Location),
                                 blast_id = simp_id(unique(x$Geo_Location)), 
                                 stringsAsFactors = FALSE) %>% 
      mutate(ADMIN  = match(blast_id, simp_id(countries$ADMIN))) %>% 
      mutate(ISO_A3 = match(blast_id, simp_id(countries$ISO_A3))) %>% 
      mutate(mapper = if_else(!is.na(ADMIN), ADMIN, ISO_A3)) %>% 
      filter(!is.na(mapper))
    
    
    
    # keep only countries in the blast results
    my_countries <- countries[country_mapper$mapper,]
    my_countries$blast_id <- country_mapper$blast_id_orig
    #print(country_mapper)
    
    
    # Add color by date
    x <- x %>% 
      # Fix collection date color
      mutate(Collection_Date2 = if_else(nchar(Collection_Date) == 7,
                                        paste0(Collection_Date, "-32"),
                                        Collection_Date)) %>% 
      mutate(fixdate = as.Date(gsub("32$", "15", Collection_Date2)))%>%
      mutate(fixdate2 = cut.Date(fixdate, 6, labels = FALSE)) %>%
      mutate(fixdate3 = cut.Date(fixdate, 6)) %>%
      mutate(col_collect = sort(unique(as.character(fixdate3)))[fixdate2]) %>% 
      # Fix Release date color
      mutate(fixdate = as.Date(Release_Date))%>%
      mutate(fixdate2 = cut.Date(fixdate, 6, labels = FALSE)) %>%
      mutate(fixdate3 = cut.Date(fixdate, 6)) %>%
      mutate(col_release = sort(unique(as.character(fixdate3)))[fixdate2]) 
    
    #------------------------------------#
    #     Ad coordinates to blaster      #
    #------------------------------------#
    #countries$blast_id
    idx <- match(x$Geo_Location, my_countries$blast_id)
    x$longitude <- my_countries$longitude[idx]
    x$latitude <- my_countries$latitude[idx]
    
    # print(colnames(x))
    # print(as.data.frame(head(x)))
    x$dots_lab <- paste(sep = "<br/>",
                        paste0("<b><a href='https://www.ncbi.nlm.nih.gov/nuccore/", 
                               x$Accession, 
                               "' target='_blank'>", 
                               x$Accession, 
                               "</a></b>"),
                        paste0("Query : ", x$qaccver),
                        paste0("Host : ", x$Host),
                        paste0("Geo Location : ", x$Geo_Location),
                        paste0("Collection Date : ", x$Collection_Date),
                        paste0("Release Date : ", as.Date(x$Release_Date)),
                        paste0("percent identity = ", x$pident),
                        paste0("evalue = ", x$evalue),
                        paste0("bitscore = ", x$bitscore),
                        paste0("Definition = ", x$GenBank_Title)
    ) 
    #print(my_countries@data)
    
    blaster_form_react$df <- x
    blaster_form_react$my_countries <- my_countries
    blaster_form_react$dots_pal <- dots_pal
    #print(blaster_form_react$df)
  }, priority = 50)
  
  
  #----------------------------------------------------------------------------#
  #                           Reactive widgets                                 #
  #----------------------------------------------------------------------------#
  
  
  
  # Country selector
  output$sel_country <- renderUI({
    selectInput(
      inputId = "sel_country",
      label = "Choose countries (default all):",
      choices = unique(na.omit(blaster_react()$Geo_Location)),
      multiple = TRUE
    )
  })
  
  # Date selector
  output$date_range <- renderUI({
    blaster_react()
    input$searchSequence
    #print(blaster_react())

    collection_months <- sort(unique(blaster_react()$collection_months))

    if (length(collection_months) == 0 | is.null(collection_months)) {
      #collection_months <- c(0,0)
      #collection_months <- NULL
      # sliderTextInput(
      #   inputId = "date_range",
      #   label = "Date Range:",
      #   choices = collection_months
      #   #selected = NULL
      # )
      NULL
    } else {
      sliderTextInput(
        inputId = "date_range",
        label = "Date Range:",
        choices = collection_months,
        selected = collection_months[c(1, length(collection_months))]
      )
    }
  })

  # observeEvent(input$searchSequence, {
  #   print("slider")
  #   updateSliderTextInput(
  #     session,
  #     inputId = "date_range",
  #     label = "Date Range:",
  #     selected = NULL,
  #     choices = NULL,
  #     from_fixed = NULL,
  #     to_fixed = NULL
  #   )
  # })
  
  
  
  
  output$totalhits <- renderValueBox({
    if (is.null(nrow(blaster_filt()))) {
      valueBox(value    = "--",
               subtitle = "Total hits",
               color    = "orange",
               icon     = icon("bullseye"),
               width    = 2)
    } else {
      valueBox(value    = nrow(blaster_react()),
               subtitle = "Total hits",
               color    = "orange",
               icon     = icon("bullseye"),
               width    = 2)
      
    }
    
    
  })
  
  output$hitsafterfil <- renderValueBox({
    if (is.null(nrow(blaster_filt()))) {
      valueBox(value    = "--", 
               subtitle = "Hits after filters",
               color    = "red", 
               icon     = icon("bullseye"),
               width    = 2)
    } else {
      valueBox(value    = nrow(blaster_filt()), 
               subtitle = "Hits after filters",
               color    = "red", 
               icon     = icon("bullseye"),
               width    = 2)
      
    }
    
    
  })
  
  #----------------------------------------------------------------------------#
  #                      Reactive widgets filters scores                       #
  #----------------------------------------------------------------------------#
  slider_bounds <- function(x){
    bounds <- c(min(blaster_react()[,x]), max(blaster_react()[,x]))
  }
  
  output$blastrf_pident <- NULL
  output$blastrf_evalue <- NULL
  output$blastrf_bitscore <- NULL
  outputOptions(output, "blastrf_pident", suspendWhenHidden = FALSE, priority = 100000)
  outputOptions(output, "blastrf_evalue", suspendWhenHidden = FALSE, priority = 100000)
  outputOptions(output, "blastrf_bitscore", suspendWhenHidden = FALSE, priority = 100000)
  
  output$blastrf_pident <- renderUI({
    req(blaster_react())
    
    bounds <- c(min(blaster_react()$pident), max(blaster_react()$pident))
    # print(blaster_react()$pident)
    # print(bounds)
    # print(signif(seq(bounds[1], bounds[2], length.out = 10), 3))
    #print(paste(c("Percent identity (pident): changed", bounds), collapse = ","))
    sliderTextInput(
      inputId = "blastrf_pident",
      label = "Percent identity (pident)", 
      choices = unique(seq(bounds[1], bounds[2], length.out = 5)),
      #choices = signif(seq(bounds[1], bounds[2], length.out = 10), 3),
      selected = bounds,
      from_min = bounds[1],
      from_max =  bounds[2],
      grid = TRUE
    )
  })
  
  output$blastrf_evalue <- renderUI({
    req(blaster_react())
    
    bounds <- c(min(blaster_react()$evalue), max(blaster_react()$evalue))
    #print(bounds)
    #print(paste(c("Expectation value (E) changed: ", bounds), collapse = ","))
    sliderTextInput(
      inputId = "blastrf_evalue",
      label = "Expectation value (E)", 
      choices = unique(seq(bounds[1], bounds[2], length.out = 5)),
      #choices = signif(seq(bounds[1], bounds[2], length.out = 10), 3),
      selected = bounds,
      from_min = bounds[1],
      from_max =  bounds[2],
      grid = TRUE
    )
  })
  
  output$blastrf_bitscore <- renderUI({
    #print(blaster_react())
    req(blaster_react())
    bounds <- c(min(blaster_react()$bitscore), max(blaster_react()$bitscore))
    #print(bounds)
    #print(paste(c("Bitscore changed: ", bounds), collapse = ","))
    sliderTextInput(
      inputId = "blastrf_bitscore",
      label = "bitscore",
      choices = unique(seq(bounds[1], bounds[2], length.out = 5)),
      #choices = signif(seq(bounds[1], bounds[2], length.out = 10), 3),
      selected = bounds,
      from_min = bounds[1],
      from_max =  bounds[2],
      grid = TRUE
    )
  })
  
  
  
  observeEvent({
    blaster_react()
  }, {
    req(blaster_react())
    
    filscores_reac$pident <- slider_bounds("pident")
    filscores_reac$evalue <- slider_bounds("evalue")
    filscores_reac$bitscore <- slider_bounds("bitscore")
    
  }, priority = 10000)
  
  observeEvent({
    input$blastrf_pident
    input$blastrf_evalue
    input$blastrf_bitscore
  }, {
    req(blaster_react())
    if(!all(c(filscores_reac$pident, filscores_reac$evalue, filscores_reac$bitscore) ==
           c(input$blastrf_pident, input$blastrf_evalue, input$blastrf_bitscore))){
      print("update slider thresholds from sliders")
      filscores_reac$pident <- input$blastrf_pident
      filscores_reac$evalue <- input$blastrf_evalue
      filscores_reac$bitscore <- input$blastrf_bitscore
    }
    
    
    #print(paste("slider", filscores_reac$bitscore))
    
    #print("end update")
    
  }, priority = 10000)
  
  #observeEvent(blaster_react(), print("blaster_react changed"))
  #----------------------------------------------------------------------------#
  #                           Filter Results                                   #
  #----------------------------------------------------------------------------#
  
  fil_by_location <- function() {
    if (length(input$sel_country) == 0) {
      #input$sel_country
      unique(blaster_react()$Geo_Location)
    } else {
      input$sel_country
    }
  }
  
  fil_by_collection_date <- function() {
    if (length(input$date_range) == 0) {
      #input$sel_country
      c(min(blaster_react()$collection_months), max(blaster_react()$collection_months))
    } else {
      input$date_range
    }
  }
  
  fil_by_score_blastrf_pident <- function() {
    if (length(input$blastrf_pident) == 0) {
      c(min(blaster_react()$pident), max(blaster_react()$pident))
    } else {
      input$blastrf_pident
    }
  }
  
  fil_by_score_blastrf_evalue <- function() {
    if (length(input$blastrf_evalue) == 0) {
      c(min(blaster_react()$evalue), max(blaster_react()$evalue))
    } else {
      input$blastrf_evalue
    }
  }
  
  fil_by_score_blastrf_bitscore <- function() {
    if (length(input$blastrf_pident) == 0) {
      c(min(blaster_react()$bitscore), max(blaster_react()$bitscore))
    } else {
      input$blastrf_bitscore
    }
  }
  
  # observeEvent({
  #   #blaster_form_react$df
  #   #input$date_range
  #   
  #   
  #   filscores_reac$pident
  #   # filscores_reac$evalue
  #   # filscores_reac$bitscore
  #   
  # }, {
  #   print("trigger")
  # })
  
  observeEvent({
    blaster_form_react$df
    input$score_id
    input$sel_area_col
    input$sel_country
    input$date_range
    # input$blastrf_pident
    # input$blastrf_evalue
    # input$blastrf_bitscore
    filscores_reac$pident
    filscores_reac$evalue
    filscores_reac$bitscore
    
  }, {
    #print(fil_by_score_blastrf_pident())
    #print(fil_by_score_blastrf_evalue())
    #print(dim(blaster_filt()))
    #print(dim(blaster_form_react$df))
    
    # blaster_form_react$df
    # input$sel_country
    # input$date_range
    # input$blastrf_pident
    # input$blastrf_evalue
    # input$blastrf_bitscore
    
    
    req(blaster_form_react$df)
    # print(paste(Sys.time(), "filter"))
    #print(c("Bitscore : ", filscores_reac$bitscore))
    # x <- blaster_form_react$df %>%
    #   filter(Geo_Location %in% fil_by_location()) %>%
    #   filter(collection_months >= fil_by_collection_date()[1] &
    #            collection_months <= fil_by_collection_date()[2]) %>% 
    #   # Filter by pident
    #   filter(pident >= fil_by_score_blastrf_pident()[1] &
    #            pident <= fil_by_score_blastrf_pident()[2]) %>% 
    #   # Filter by evalue
    #   filter(evalue >= fil_by_score_blastrf_evalue()[1] &
    #            evalue <= fil_by_score_blastrf_evalue()[2]) %>%
    #   # Filter by bitscore
    #   filter(bitscore >= fil_by_score_blastrf_bitscore()[1] &
    #            bitscore <= fil_by_score_blastrf_bitscore()[2])
    score_id <- names(score_ids)[score_ids %in% input$score_id]
    
    x <- blaster_form_react$df %>%
      filter(Geo_Location %in% fil_by_location()) %>%
      filter(collection_months >= fil_by_collection_date()[1] &
               collection_months <= fil_by_collection_date()[2]) %>% 
      # Filter by pident
      filter(pident >= filscores_reac$pident[1] &
               pident <= filscores_reac$pident[2]) %>% 
      # Filter by evalue
      filter(evalue >= filscores_reac$evalue[1] &
               evalue <= filscores_reac$evalue[2]) %>%
      # Filter by bitscore
      filter(bitscore >= filscores_reac$bitscore[1] &
               bitscore <= filscores_reac$bitscore[2]) %>% 
      mutate(currentscore = !! sym(score_id))
    
    #print(x)
    if (nrow(x) == 0) {
      blaster_filt(NULL)
    } else {
      blaster_filt(x)
    }
    
    
  }, priority = 40)
  
  
  
  #----------------------------------------------------------------------------#
  #                                       Map                                  #
  #----------------------------------------------------------------------------#
  
  ## Interactive Map ###########################################
  
  # # Create the map
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     setView(42, 16, 2) %>% 
  #     addProviderTiles(providers$CartoDB.DarkMatter)
  # })
  
  # output$empty_map <- renderLeaflet({
  #   leaflet() %>%
  #     setView(42, 16, 2) %>% 
  #     addProviderTiles(providers$CartoDB.DarkMatter)
  # })
  
  
  #----------------------------------------------------------------------------#
  #                              Color areas                                   #
  #----------------------------------------------------------------------------#
  
  
  #----------------------------------------------------------------------------#
  #                              Add clusters                                  #
  #----------------------------------------------------------------------------#

  observeEvent({
    blaster_filt()
    input$specialclust_switch
  }, {
    print(paste(Sys.time(), "Add clusters special: ", input$specialclust_switch))
    req(blaster_filt())
    
    #print(dim(blaster_filt()))
    #---------------------------------------------------------#
    #                     Add Normal clusters                 #
    #---------------------------------------------------------#
    if ( !input$specialclust_switch) {
      #if (!is.null(blaster_filt()) & !input$specialclust_switch) {
      # print("map clusters")
      #print(dim(blaster_filt()))
      #print(blaster_filt())
      
      blaster_map <- blaster_filt()
      dots_pal <- blaster_form_react$dots_pal
      
      # print(summary(blaster_map$radius))
      # print(summary(blaster_map$radiusfix))
      # #print(dots_pal)
      # print(blaster_map %>%
      #         filter(Geo_Location == "Nigeria") %>%
      #         select(radius, radiusfix))
      
      leafletProxy("map", data = blaster_map) %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        addCircleMarkers(lng = blaster_map$longitude,
                         lat = blaster_map$latitude,
                         radius = blaster_map$radiusfix,
                         color = ~dots_pal(blaster_map$radius),
                         clusterOptions = markerClusterOptions(
                           spiderfyDistanceMultiplier=1.2
                         ),
                         popup = blaster_map$dots_lab,
                         fillOpacity = 1)
      
      output$specialclust_legend <- renderUI({
        NULL
        
      })
      
      #---------------------------------------------------------#
      #                     Add Special clusters                #
      #---------------------------------------------------------# 
    } else if (input$specialclust_switch) {
      # print("map clusters")
      #print(dim(blaster_filt()))
      #print(blaster_filt())
      
      score_id <- names(score_ids)[score_ids %in% input$score_id]
      #print(score_id)
      blaster_map <- blaster_filt() %>% 
        mutate(try = !! sym(score_id)) %>% 
        mutate(score_scaled = ntile(!! sym(score_id), 10)) %>% 
        mutate(score_scaled = factor(score_scaled, sort(unique(score_scaled)))) #%>% 
        # mutate(radiusfix = if_else(radiusfix == 12, 40, as.numeric(radiusfix)) ) %>% 
        # mutate(radiusfix = factor(radiusfix, levels = sort(unique(radiusfix))))

      
      dots_pal <- blaster_form_react$dots_pal
      
      #Generate the javascript
      jsscript3 <- map_special_cluster(blaster_map$score_scaled)
      
      

      output$map <- renderLeaflet({
        leaflet() %>%
          setView(42, 16, 2) %>% 
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          clearMarkers() %>%
          addCircleMarkers(data = blaster_map,
                           lng = ~longitude,
                           lat = ~latitude,
                           group = ~score_scaled,
                           radius = ~radiusfix,
                           color = ~dots_pal(blaster_map$radius),
                           clusterOptions = markerClusterOptions(
                             spiderfyDistanceMultiplier=3,
                             iconCreateFunction = JS(jsscript3)
                           ),
                           popup = ~dots_lab,
                           fillOpacity = 1) 
          # addCircleMarkers(data = blaster_map,
          #                  lng = ~longitude,
          #                  lat = ~latitude,
          #                  group = ~radius,
          #                  radius = ~radiusfix,
          #                  color = ~dots_pal(blaster_map$radius),
          #                  clusterOptions = markerClusterOptions(
          #                    spiderfyDistanceMultiplier=1.2,
          #                    iconCreateFunction = JS(jsscript3)
          #                  ),
          #                  popup = ~dots_lab,
          #                  fillOpacity = 1) 
      })
      
      
      #---------------------------------------------------------#
      #               Add Special clusters Legend               #
      #---------------------------------------------------------# 
      output$gg_spclu_leg <- renderPlot({
        req(blaster_map)
        
        # print(blaster_map %>%
        #         group_by(score_scaled) %>% 
        #         summarise(score = mean(!! sym(score_id))) )
        
        # Input data is filtered hits table
        blaster_map %>%
          group_by(score_scaled) %>% 
          summarise(score = mean(!! sym(score_id))) %>% 
          #mutate(score_scaled = score_scaled*10) %>% 
          # mutate(collection_months = factor(collection_months, 
          #                                   levels = sort(unique(collection_months)))) %>% 
          ggplot(aes(x = score_scaled, y = score, fill = score_scaled)) +
          geom_bar(stat = "identity") +
          scale_fill_viridis_d(option = "D", end = 0.9) +
          theme_bw() + 
          ylab(score_id) +
          xlab("score ranks") +
          #theme_dark() + 
          #scale_fill_tron()  +
          theme(#legend.position = "none",
            #text = element_text(colour = "white"),
            #axis.text.x = element_text(colour = "white"),
            #axis.text.y = element_text(colour = "white"),
            #plot.background =element_rect(fill = "#2D2D2D"),
            #panel.background = element_rect(fill = "#2D2D2D"),
            axis.line=element_blank(),
            
            axis.ticks=element_blank(),
            #axis.title.x=element_blank(),
            #axis.title.y=element_blank(),
            panel.grid = element_blank(),
            legend.position="none") 
      },
      #width  = 100, 
      height = 90
      )
      
      output$specialclust_legend <- renderUI({
        #h4("Cluster legend", align = "center")
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE,
                      #top = 60,
                      left = "auto", right = 50, bottom = "auto",
                      width = 200, height = 150,

                      h4("Cluster legend", align = "center"),
                      plotOutput(outputId = "gg_spclu_leg"),
                      
                      top = "75%",
        )
      })
      
      
    }
    
    
  }, priority = 1)
    
  # observeEvent(blaster_filt(), {
  #   if (!is.null(blaster_filt())) {
  #     # print("map clusters")
  #     # print(dim(blaster_filt()))
  #     
  #     blaster_map <- blaster_filt() 
  #     dots_pal <- blaster_form_react$dots_pal
  #     
  #     # print(summary(blaster_map$radius))
  #     # print(summary(blaster_map$radiusfix))
  #     # #print(dots_pal)
  #     # print(blaster_map %>% 
  #     #         filter(Geo_Location == "Nigeria") %>% 
  #     #         select(radius, radiusfix))
  # 
  #     leafletProxy("map", data = blaster_map) %>%
  #       clearMarkerClusters() %>%
  #       clearMarkers() %>%
  #       addCircleMarkers(lng = blaster_map$longitude,
  #                        lat = blaster_map$latitude,
  #                        radius = blaster_map$radiusfix,
  #                        color = ~dots_pal(blaster_map$radius),
  #                        clusterOptions = markerClusterOptions(
  #                          spiderfyDistanceMultiplier=1.2
  #                        ),
  #                        popup = blaster_map$dots_lab,
  #                        fillOpacity = 1)
  #   }
  # 
  # 
  # }, priority = 1)
  
  
  
  ##--------------------------------------------------------------------------##
  ##              Get main table and fitler according to options              ##
  ##--------------------------------------------------------------------------##
  ### Data Explorer ###########################################
  
  output$blaster_ui <- DT::renderDataTable({
    req(blaster_filt())
    
    df <- blaster_filt() %>% 
      mutate(Release_Date = as.Date.character(Release_Date)) %>%
      select(Accession, pident, evalue, bitscore, Geo_Location, Host,
             Release_Date, Collection_Date, length, mismatch, gapopen,
             qstart, qend, sstart, send, Length, Isolation_Source, Species) %>% 
      filter(Geo_Location %in% fil_by_location()) 
    
    
    action <- DT::dataTableAjax(session, df, outputId = "blaster_ui")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE, class = "nowrap display")
  })
  
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadBlaster <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "SARScov2_alignment.tsv"
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(blaster_filt(), file, sep = "\t",
                  row.names = FALSE)
    }
  )
  
  
  
  
  ##--------------------------------------------------------------------------##
  ##                       Barplot of hits per month                          ##
  ##--------------------------------------------------------------------------##
  
  output$gg_data_months <- renderPlot({
    req(blaster_filt())
    # Input data is filtered hits table
    blaster_filt() %>% 
      mutate(collection_months = factor(collection_months, 
                                        levels = sort(unique(collection_months)))) %>% 
      ggplot(aes(x = collection_months, fill = collection_months)) +
      geom_bar(stat = "count") +
      scale_fill_viridis_d(option = "B", begin = 0.4, end = 0.8) +
      theme_dark() + 
      #scale_fill_tron()  +
      theme(#legend.position = "none",
        text = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.background =element_rect(fill = "#2D2D2D"),
        panel.background = element_rect(fill = "#2D2D2D"),
        axis.line=element_blank(),
        
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        panel.grid = element_blank(),
        legend.position="none") 
  },
  #width  = 100, 
  height = 80
  )
  
  
}
