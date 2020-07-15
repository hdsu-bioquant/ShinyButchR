library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


dashboardPage(
  title="ShinyWurst",
  #----------------------------------------------------------------------------#
  #                                Header                                      #
  #----------------------------------------------------------------------------#
  dashboardHeader(
    title = "ShinyWurst",
    # title = tags$a(href=".",
    #                tags$img(src="map_my_corona_logo.png",height="50px")),
    tags$li(class = "dropdown",
            tags$a(href="https://www.hdsu.org/", target="_blank",
                   tags$style(".main-header {max-height: 55px}"),
                   tags$style(".main-header .logo {height: 55px}"),
                   tags$img(height = "30px", alt="SNAP Logo", src="logo_hdsu.png")
            )
    )
    
  ),
  
  #----------------------------------------------------------------------------#
  #                                Sidebar                                     #
  #----------------------------------------------------------------------------#
  dashboardSidebar(
    
    sidebarMenu(
      id = "mainmenu",
      br(),
      h6("ShinyWurst is an app to run ", align = "center"),
      h6("Non-Negative Matrix Factorization (NMF) ", align = "center"),
      h6("on a small input matrix", align = "center"),
      h6("Using the Bratwurst package", align = "center"),
      tags$hr(),
      
      
      h4("Factorize Matrix", align = "center"),
      h6("from .csv or .RDS files", align = "center"),
      
      menuItem(
        "Data and annotation upload ",
        #id = "nmfinput",
        tabName = "nmfinput",
        icon = icon("search")
      ),
      
      tags$hr(),
      
      
      menuItem(
        "NMF plots",
        tabName = "nmfplots",
        icon = icon("bar-chart")
      ),
      
      menuItem(
        "Save results",
        #id = "nmfinput",
        tabName = "saveres",
        icon = icon("archive")
      ),
      
      tags$hr(),
      menuItem(
        "FAQ",
        tabName = "faq",
        #icon = icon("user-secret")
        icon = icon("question-circle")
      ),
      tags$hr()
      
    )
  ),
  
  #----------------------------------------------------------------------------#
  #                              dashboardBody                                 #
  #----------------------------------------------------------------------------#
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
        )
      )
    ),
    
    tabItems(
      #------------------------------------------------------------------------#
      #                          NMF input and params                          #
      #------------------------------------------------------------------------#
      tabItem(
        tabName = "nmfinput",
        
        fluidRow(
          box(
            id = "uploadmatrix",
            width = 8,
            height = 520,
            title ="Matrix upload",
            status = "warning",
            background = "black",
            #tags$hr(),
            h4("Upload a matrix in a csv or RDS file",
               align = "center"),
            # Input: Select a file ----
            fileInput("file1", "Choose a .csv or .RDS File",
                      multiple = FALSE,
                      accept = c(".RDS", ".csv")),
            
            
            uiOutput("inputmatrix_printdim"),
            uiOutput("inputmatrix_printout"),
            
            actionBttn(
              inputId = "clear_inputMatrix",
              label = "Clear",
              style = "minimal",
              size = "xs",
              color = "default"
            ),

            
            h6(actionBttn(
              inputId = "load_demo",
              label = "Demo",
              style = "jelly",
              size  = "md",
              color = "royal"
            ), align = "center"),
            

            h6("Click Demo ", align = "center"),
            h6("to load Corces-Buenrostro AML dataset", align = "center"),

          ),

          box(
            id = "nmfparams",
            width = 4,
            height = 520,
            title = "NMF params",
            status = "warning",
            background = "black",

            numericInput(
              inputId = "params_kmin",
              label   = "Minimum factorization rank:",
              value   = 2,
              min     = 2,
              max     = 30
            ),

            numericInput(
              inputId = "params_kmax",
              label   = "Maximum factorization rank:",
              value   = 3,
              min     = 2,
              max     = 30
            ),

            selectInput(
              inputId = "params_method",
              label = "Select factorization method:",
              choices = c("NMF", "GRNMF_SC"),
              selected = "NMF",
              multiple = FALSE
            ),

            numericInput(
              inputId = "params_ninits",
              label   = "Number of initializations:",
              value   = 2,
              min     = 1,
              max     = 10
            ),

            numericInput(
              inputId = "params_convthrs",
              label   = "Convergence threshold:",
              value   = 40,
              min     = 10,
              max     = 100
            )
          )

        ),


        fluidRow(
          box(
            id = "uploadannot",
            width = 8,
            height = 520,
            title ="Annotation upload",
            status = "warning",
            background = "black",
            #tags$hr(),
            h4("Upload a table with annotation data in a csv or RDS file",
               align = "center"),
            h4("The first column should match the column names in the input matrix",
               align = "center"),
            # Input: Select a file ----
            fileInput("file_annot", "Choose a .csv or .RDS File",
                      multiple = FALSE,
                      accept = c(".RDS", ".csv")),
            
            #uiOutput("inputannot_selcols"),
            uiOutput("inputannot_printdim"),
            uiOutput("inputannot_printout")
            
            #h6("The first column is not used ", align = "center"),
            
          ),
          
          box(
            title = h1("Start NMF", align = "center"),
            width = 4, 
            height = 520,
            solidHeader = TRUE,
            status = "success",
            background = "black",
            
            h4("Click 'Submit' button to start running NMF 
               in the uploaded matrix and with selected parameters",
               align = "center"),
            
            # actionBttn(
            #   inputId = "startNMF",
            #   label = "Submit",
            #   style = "jelly",
            #   size  = "lg",
            #   color = "danger"
            # )
            
            h1(actionBttn(
              inputId = "startNMF",
              label = "Submit",
              style = "jelly",
              size  = "lg",
              color = "danger"
            ),
            align = "center")
          )
        )


      ),
      
      #------------------------------------------------------------------------#
      #                                   FAQ                                  #
      #------------------------------------------------------------------------#
      tabItem(
        tabName = "faq",
        fluidRow(
          box(
            id = "help_main",
            width = 5,
            height = 400,
            title = h2("ShinyWurst", style="text-align: center;") ,
            status = "warning",
            background = "black",
            tags$hr(),
            h4("ShinyWurst is an app to run 
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
            h4("ShinyWurst was developed by the 
            Biomedical Genomics Group @ Health Data Science Unit 
            at the BioQuant Center and Medical Faculty Heidelberg",
               align = "center"),
            h4(tags$a(href="https://www.hdsu.org/", 
                      target="_blank",
                      "Visit us here!"),
               align = "center"),
            
            tags$hr(),
            
            h1(tags$a(href="https://github.com/hdsu-bioquant/shinyWurst", 
                      target="_blank",
                      icon("github")), align = "center"),
            #h1(icon("github"), align = "center"),
            h4("If you have any suggestion please create an issue in our GitHub repository",
               tags$a(href="https://github.com/hdsu-bioquant/shinyWurst", 
                      target="_blank", 
                      "hdsu-bioquant/shinyWurst"),
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
      ),
      
      
      #------------------------------------------------------------------------#
      #                             NMF plots                                  #
      #------------------------------------------------------------------------#
      tabItem(
        tabName = "nmfplots",
        
        #----------------------------------------------------------------------#
        #                        H matrix Heatmap                              #
        #----------------------------------------------------------------------#
        fluidRow(
          sel_KUI("HHeat"),
          # box(
          #   title = "H Matrix Heatmap", 
          #   width = 3, 
          #   height = 350,
          #   solidHeader = TRUE, status = "primary",
          #   sel_KUI("HHeat"),
          #   #sel_KUI("HHeat-sel_k"),
          #   #sel_KUI("sel_K"),
          #   #sel_KUI("sel_K2"),
          #   #uiOutput("sel_K"),
          #   
          #   prettySwitch(
          #     inputId = "hmatheat_showcol",
          #     label = "Column names",
          #     value = TRUE,
          #     status = "success",
          #     fill = TRUE
          #   ), 
          #   
          #   prettySwitch(
          #     inputId = "hmatheat_cluster_rows",
          #     label = "Cluster rows",
          #     value = TRUE,
          #     status = "success",
          #     fill = TRUE
          #   ),
          #   
          #   prettySwitch(
          #     inputId = "hmatheat_cluster_cols",
          #     label = "Cluster columns",
          #     value = TRUE,
          #     status = "success",
          #     fill = TRUE
          #   ),
          #   
          #   prettySwitch(
          #     inputId = "hmatheat_annot",
          #     label = "Show annotation",
          #     value = TRUE,
          #     status = "success",
          #     fill = TRUE
          #   ),
          #   
          #   uiOutput("inputannot_selcols")
          #   
          # ),
          box(
            #title = "Title 2", 
            width = 9, 
            height = 350,
            solidHeader = TRUE,
            #plotOutput(outputId = "plot_hmatrixheat")
            HHeatmapServerUI("HHeat")
          )
        ),
        
        #----------------------------------------------------------------------#
        #                        Recovery Plots                                #
        #----------------------------------------------------------------------#
        fluidRow(
          box(
            title = "Recovery Plots", 
            width = 3, 
            height = 350,
            solidHeader = TRUE, status = "primary",
            uiOutput("sel_Krecov"),
            
            uiOutput("inputannot_selcols_recov")
            
          ),
          box(
            #title = "Title 2", 
            width = 9, 
            height = 350,
            solidHeader = TRUE,
            plotOutput(outputId = "plot_recoveryplots")
          )
        ),
        
        
        #----------------------------------------------------------------------#
        #                    Optimal factorization rank                        #
        #----------------------------------------------------------------------#
        
        
        fluidRow(
          box(
            title = "Optimal factorization rank", 
            width = 3, height = 330,
            solidHeader = TRUE, status = "primary",
            h5("Based on the results of the factorization quality metrics, 
               the optimal number of signatures (k) ", 
               align = "center"),
            uiOutput("print_optK"),
            
            h5("Minize the Frobenius error, 
               the coefficient of variation and the mean Amari distance", align = "center"),
            h5("Maximize the sum and mean silhouette width 
               and the cophenic coefficient.", align = "center")
          ),
          box(
            width = 9, height = 330,
            solidHeader = TRUE,
            plotOutput(outputId = "plot_nmfoptk")
          )
        ),
        
        #----------------------------------------------------------------------#
        #                              Riverplot                               #
        #----------------------------------------------------------------------#
        
        fluidRow(
          box(
            title = "NMF riverplot", 
            width = 3, height = 430,
            solidHeader = TRUE, status = "primary",
            h5("Riverplot or Sankey diagram showing the similarity 
               between signatures across factorization ranks", 
               align = "center"),
            uiOutput("sel_riverRange"),
            
            sliderTextInput(
              inputId = "sel_edges_cutoff",
              label = "cutoff until which similarities are displayed:", 
              choices = seq(0, 1, by = 0.1),
              selected = 0.2,
              grid = TRUE
            )
            
            
          ),
          box(
            #title = "Title 2", 
            width = 9, height = 430,
            solidHeader = TRUE,
            plotOutput(outputId = "plot_riverplot")
          )
        )
      ),
      
      
      tabItem(
        tabName = "saveres",
        
        
        fluidRow(
          box(
            title = h2("Save NMF object"), 
            width = 6, 
            height = 500,
            solidHeader = TRUE, 
            status = "warning",
            background = "black",
            #uiOutput("sel_K"),
            #uiOutput("inputannot_selcols")
            h3("Save NMF object to use later in R", align = "center"),
            
            downloadButton('downloadNMFobject', 'Download'),
            
            h3("This object is complatible with the latest 
               version of Bratwurst.", align = "center"),
            h3("To install in R use:", align = "center"),
            h5("devtools::install_github('hdsu-bioquant/bratwurst', 
               ref='dev_hdsu_lite')", align = "center")
            
            
            
          ),
          box(
            title = h2("Save H and W matrices"), 
            status = "warning",
            background = "black",
            width = 6, 
            height = 500,
            solidHeader = TRUE,
            h3("Save the H matrix or the W matrix 
               in csv or RDS format", align = "center"),
            
            #select K to download
            uiOutput("download_sel_K"),
            
            radioGroupButtons(
              inputId = "download_matrixwhich",
              label = "Select Matrix", 
              choices = c("H", "W"),
              selected = "H",
              justified = FALSE,
              checkIcon = list(
                yes = icon("ok", 
                           lib = "glyphicon"))
            ),
            
            radioGroupButtons(
              inputId = "download_matrixformat",
              label = "Select format", 
              choices = c(".RDS", ".csv"),
              selected = ".RDS",
              justified = FALSE,
              checkIcon = list(
                yes = icon("ok", 
                           lib = "glyphicon"))
            ),
            
            
            
            downloadButton('downloadHmatrix', 'Download')
            
          )
        )
      )
      
      
    )
  ),
  skin = "black"
)



