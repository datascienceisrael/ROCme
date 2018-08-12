# set working directory to current file location
# rstudioapi::getActiveDocumentContext()
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load utilities
source("init.R")

# user interface code --------------------
# header
header <- dashboardHeader(title = "ROCme")

# sidebar layout
sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",
                                        # first item - general instructions
                                        menuItem("Info", tabName = "instructions", icon = icon("info-circle")),
                                        # second item - upload file
                                        menuItem("Data", tabName = "uploadData", icon = icon("table")),
                                        # third item - global KPIs
                                        menuItem("Global Metrics", tabName = "global_metrics", icon = icon("globe")),
                                        # fourth item - local KPIs
                                        menuItem("Local Metrics", tabName = "local_est", icon = icon("binoculars")),
                                        # for debugging
                                        textOutput("res")
))

body <- dashboardBody(
  # custom font for the header
  tags$head(tags$style(HTML('
                            .main-header .logo {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 24px;
                            }'))),
  ### changing theme
  # shinyDashboardThemes(
  #   theme = "slate"
  # ),
  
  tabItems(
    
    # instructions Tab
    tabItem(tabName = "instructions",
            includeHTML('description.html')),
    
    # Upload data
    tabItem(tabName = "uploadData",
            # choose file
            shinydashboard::box(collapsible = TRUE, 
                                status = "warning",
                                title = "Data Upload",
                                footer = "Note: file must not exceed 50MB",
                                fileInput("datafile", "Choose file", placeholder = "No file selected",
                                          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
            
            # Explanations
            shinydashboard::box(collapsible = TRUE,
                                status = "warning",
                                includeHTML('format.html'))),
    
    # global metrics visualization
    tabItem(tabName = "global_metrics",
      tabBox(title = "Global Model Metrics",
             id = "tabset1",
             width = 12,
             # Roc curves
             tabPanel(title = "Roc Curves",
                      status = "primary",
                      
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotOutput('rocs',  height = 500)),
             # Predictions Distribution
             tabPanel(title = "Prediction Scores Distribution",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotOutput('predictions_dist',  height = 500)),
             # Predictions Bins
             tabPanel(title = "Prediction Scores Bins",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotOutput('predictions_splits',  height = 500)))
    ),
    
    # Local metrics visualization
    tabItem(tabName = "local_est",
            # Confusion Matrix
            shinydashboard::box(title = "Confusion Matrix",
                                width = 6,
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput('confPlot')),
            # Decision Criterion
            shinydashboard::box(status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                sliderInput("crit", "Decision Criterion:",
                                            min = 0.001, max = 0.999,
                                            value = 0.5, step = 0.05,
                                            sep = ",",
                                            animate = TRUE)),
            # Evluation Metrics
            shinydashboard::box(title = "Evaluation Metrics",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                tableOutput('metrics')))
  )
)

# user interface
ui <- dashboardPage(header, sidebar, body)



# code on the server side ---------------------
server <- function(input, output, session) {
  
  # set file limit to 50MB
  options(shiny.maxRequestSize = 50*1024^2)
  
  # read uploaded data file
  theData <- reactive({
    infile <- input$datafile
    if(is.null(infile))
      return(NULL)
    d <- read.csv(infile$datapath)
  })
  
  threshold <- reactive({ input$crit })
  
  # The ROC curve plot
  output$rocs <- renderPlot({  grid.arrange(plotROC( tab = theData() ),
                                            plot_aucpr( tab = theData()),
                                            ncol = 2) })
  
  # The predictions distribution
  output$predictions_dist <- renderPlot({  grid.arrange(mplot_density( tab = theData() ),
                                                        mplot_bins(tab = theData(), splits = N),
                                                        ncol = 1) })
  
  # The confusion Matrix visualization
  output$confPlot <- renderPlot({
    fourfoldplot(confMatrix(crit = threshold(), tab = theData()),
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 margin = 1,
                 main = "Confusion Matrix")
  })
  
  # The evaluation metrics
  output$metrics <- renderTable({
    evalMetrics(df = theData(), crit = threshold())
  })
 
  # prediction splits VS label
  output$predictions_splits <- renderPlot({ mplot_splits(tab = theData(), splits = 5) })
}

# Run the application
shinyApp(ui = ui, server = server)
