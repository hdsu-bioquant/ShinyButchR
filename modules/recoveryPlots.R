#------------------------------------------------------------------------------#
#                        Recovery plots Server & UI                            #
#------------------------------------------------------------------------------#
recovplotsUI <- function(id) {
  tagList(uiOutput(NS(id, "title_sel_K")),
          plotOutput(NS(id, "plot_recoveryplots")))
  #plotOutput(NS(id, "plot_recoveryplots"))
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
        req(input$sel_K %in% nmf_obj()@OptKStats$k)
        
        k <- as.numeric(input$sel_K)
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




##----------------------------------------------------------------------------##
##                         Recovery plots function                            ##
##----------------------------------------------------------------------------##
auc <- function(rnk.list, max = NULL) {
  aux <-  sapply(rnk.list,function(rnk) {
    if (is.null(max)) {
      max <-  max(rnk)
    } 
    rnk <-  sort(rnk)
    X <-  0
    i <-  1
    ngenes <-  length(rnk)
    while ((rnk[i] <= max) && (i <= length(rnk))) {
      X <-  X + max -rnk[i]
      i <-  i+1
    }
    #print(rnk)
    #print(class(rnk))
    rauc <-  X/(i-1)/max
    #print(rauc)
    rauc
  })
  return(aux)
}

# input named factor/character
# List of named factors/character
# data frame, col names of selected annotation. Use first column or row names as IDss
recovery_plot <- function(h, annot){
  # Add sig IDs if missing
  if (is.null(rownames(h))) {
    rownames(h) <- paste0('Sig ',1:nrow(h))
  }
  # check i annot lenght == h
  
  if (is.factor(annot) | is.character((annot))) {
    annot_list <- list(main_annot = as.factor(annot))
  } else {
    stop("Not a valid annotation input")
  }
  n_samples  <- ncol(h)
  #which.a = annotID
  #annot.factor <- annot[,annotID]
  
  
  ## -------------------------------------------------------------------##
  ##                        Find ranks                                  ##
  ##--------------------------------------------------------------------##
  # cycle all annots
  ALL_RNKS_list <- lapply(annot_list, function(annot_factor){
    # cycle annot levels
    lIds <- setNames(levels(annot_factor), levels(annot_factor))
    ALL_RNKS <-  lapply(lIds,function(l) {
      # cycle h matrix rows and find ranks
      lapply(setNames(1:nrow(h), rownames(h)),function(i) {
        exp   <-  sort(h[i,],decreasing=TRUE) # sorted exposure
        i_rnk <-  match(names(annot_factor)[annot_factor==l], names(exp))
        sort(i_rnk[!is.na(i_rnk)]) # keep steps/ranks
      })
      #print(RNKS)
      #return(RNKS)
    })
    ALL_RNKS
  })
  
  ## -------------------------------------------------------------------##
  ##                  Find AUC and P-value                              ##
  ##--------------------------------------------------------------------##
  # cycle all annots
  AUC_list <- lapply(ALL_RNKS_list, function(ALL_RNKS){
    AUC_singleannot <- lapply(ALL_RNKS,function(r) {
      # AUC random set
      AUC_RAND <- do.call("rbind",lapply(r, function(x) {
        l = lapply(1:500,function(i) {
          sample(1:n_samples, length(x))
        })
        aux = auc(l, max = n_samples)
        return(c(mean(aux), sd(aux)))
      }))
      
      # AUC
      #AUC <-  lapply(ALL_RNKS, auc, max = n_samples)
      AUC <-  auc(r, max = n_samples) 
      #print(AUC)
      
      # Find P - value
      AUC_df <- data.frame(AUC_RAND, AUC) 
      colnames(AUC_df) = c('mean','sd','val')
      AUC_df <- AUC_df %>% 
        tibble::rownames_to_column("SignatureID") %>% 
        mutate(z = (val - mean)/sd) %>% 
        mutate(p = ifelse(z>0, pnorm(z, lower.tail=FALSE), pnorm(z))) 
      
      #Return randon and AUC - P-val
      return(AUC_df)
    })
    bind_rows(AUC_singleannot, .id = "Annotation_level")
  })
  AUC_allannot <- bind_rows(AUC_list, .id = "Annotation")
  
  # Add min and max to rank, for step plot
  # cycle all annots
  ALL_RNKS_list <- lapply(ALL_RNKS_list, function(ALL_RNKS){
    # cycle annot levels
    lapply(ALL_RNKS, function(x){
      # cycle h matrix rows and find ranks
      lapply(x, function(xi) c(0, xi, n_samples))
    })
  })
  #print(ALL_RNKS_list)
  
  # Bind ranks and p vals (long data frame - p val repeated)
  ALL_RNKS_df <- bind_rows(lapply(ALL_RNKS_list, 
                                  bind_rows,.id = "Annotation_level"), 
                           .id = "Annotation") %>% 
    pivot_longer(-c("Annotation",  "Annotation_level"),  names_to = "SignatureID", values_to = "Rank") %>% 
    left_join(AUC_allannot, by = c("Annotation", "Annotation_level", "SignatureID"))
  
  gg_recov <- ALL_RNKS_df %>% 
    group_by(Annotation, Annotation_level, SignatureID ) %>% 
    mutate(Frequency = c(seq(0, 1, length.out = n()-1), 1)) %>% # all y axis step
    mutate(issignif = p < 0.05) %>% 
    
    ggplot(aes(x = Rank, y = Frequency, color = SignatureID,
               linetype = issignif, size = issignif)) +
    
    # geom_step(data = function(x){x %>% filter(!issignif)}, size  = 0.5) +
    # geom_step(data = function(x){x %>% filter(issignif)}, size  = 1.5) +
    geom_step() +
    
    geom_abline(intercept = 0, slope = 1/n_samples) +
    #facet_wrap(Annotation ~ Annotation_level) +
    facet_wrap(.~Annotation_level) +
    # chance line style
    scale_linetype_manual(name = c("Significant p-val<0.05"), 
                          values = c("TRUE" = 1, "FALSE" = 2)) +
    scale_size_manual(name = c("Significant p-val<0.05"), 
                      values = c("TRUE" = 1, "FALSE" = 0.5)) +
    #theme_bw() +
    theme_cowplot() +
    panel_border(color = "grey40", size = 1, linetype = 1,
                 remove = FALSE)
  
  
  #return(ALL_RNKS_df)
  return(gg_recov)
}

# ##----------------------------------------------------------------------------##
# ##                                Recovery plots                              ##
# ##----------------------------------------------------------------------------##
# 
# 
# recov_df <- recovery_plot(hmatrix_norm_ids, setNames(rna_annotation$Celltype, rna_annotation$sampleID))
# 
# 
# 
# #theme(panel.grid = element_blank())