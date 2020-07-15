faqUI <- function(id) {
  tabItem(
    tabName = "faq",
    fluidRow(
      box(
        id = "help_main",
        width = 5,
        height = 400,
        title = h2("ShinyButchR", style="text-align: center;") ,
        status = "warning",
        background = "black",
        tags$hr(),
        h4("ShinyButchR is an app to run 
               Non-Negative Matrix Factorization (NMF) 
               on a small input matrix
               using the Bratwurst package", align = "center"),
        h4("You can explore the results in the NMF plots tab, or download 
               the resulting H and W matrices in the save results tab.",
           align = "center")
      ),
      #tags$style(type = "text/css", "#help_hdsu {height: calc(100vh ) !important;}"),
      box(
        id = "help_hdsu",
        width = 7, 
        height = 400,
        title = img(src="logo_hdsu.png", width="100%"),
        #status = "warning",
        background = "black",
        #tags$hr(),
        h4("ShinyButchR was developed by the 
            Biomedical Genomics Group @ Health Data Science Unit 
            at the BioQuant Center and Medical Faculty Heidelberg",
           align = "center"),
        h4(tags$a(href="https://www.hdsu.org/", 
                  target="_blank",
                  "Visit us here!"),
           align = "center"),
        
        tags$hr(),
        
        h1(tags$a(href="https://github.com/hdsu-bioquant/ShinyButchR", 
                  target="_blank",
                  icon("github")), align = "center"),
        #h1(icon("github"), align = "center"),
        h4("If you have any suggestion please create an issue in our GitHub repository",
           tags$a(href="https://github.com/hdsu-bioquant/ShinyButchR", 
                  target="_blank", 
                  "hdsu-bioquant/ShinyButchR"),
           align = "center")
        
      )
    ),
    
    fluidRow(
      box(
        collapsible = TRUE,
        collapsed   = TRUE,
        title = h4(icon("search"),"Factorize", align = "center"),
        width = 4, 
        background = "blue",
        
        p("Factorized uploaded matrix",
          align = "center")
      ),
      
      box(
        collapsible = TRUE,
        collapsed   = TRUE,
        title = h4(icon("bar-chart"), "NMF plots", align = "center"),
        width = 4,
        background = "blue",
        p("Displays the optimal factorization rank metrics"),
        p("Displays a heatmap of the H matrix for each factorization rank"),
        p("Displays a riverplot of all factorization ranks")
      ),
      
      box(
        collapsible = TRUE,
        collapsed   = TRUE,
        title = h4(icon("cogs"), "Save results", align = "center"),
        width = 4,
        background = "red",
        p("saves all the results as RDS objects")
        
      )
    )
  )
}




