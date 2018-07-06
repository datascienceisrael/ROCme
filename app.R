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
                                        menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
                                        # second item - upload file
                                        menuItem("Data", tabName = "uploadData", icon = icon("table")),
                                        # third item
                                        menuItem("Analysis", tabName = "analysis", icon = icon("binoculars")),
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
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  
  tabItems(
    # instructions Tab
    tabItem(tabName = "instructions",
            fluidPage(h3("This App is a Tool to Analyse Binay Classification Results\n"), br(),
                      h4("  + Modify Decision Criterion Dynamically and Examine Implications\n"), br(),
                      h4("  + Diagnose AUC plot\n"), br(),
                      h4("  + Check the standard metrics like sensitivity, specificity, precision and recall\n"), br(),
                      h5("Any Ideas To Expand Functionality? Let Me Know :)"))),
    # Upload data
    tabItem(tabName = "uploadData",
            # choose file
            gradientBox(
              title = "Data Upload",
              icon = "fa fa-upload",
              gradientColor = "teal",
              boxToolSize = "s",
              closable = FALSE,
              footer = "Note: file must not exceed 30MB",
              fileInput("datafile", "Choose file", placeholder = "No file selected",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
            
            # Explanations
            gradientBox(gradientColor = "teal",
                        closable = TRUE,
                        includeHTML('format.html'))),
    # Analysis
    tabItem(tabName = "analysis",
            # ROC curve
            shinydashboard::box(title = "Roc Curves",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput('roc')),
            # Debugging
            shinydashboard::box(title = "Confusion Matrix",
                                with = 3,
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
                                            value = 0, step = 0.05,
                                            sep = ",",
                                            animate = TRUE)),
            # Evluation Metrics
            shinydashboard::box(title = "Evaluation Metrics",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                tableOutput('metrics'))
    )
  )
)

# user interface
ui <- dashboardPage(header, sidebar, body)



# code on the server side ---------------------
server <- function(input, output, session) {
  
  # set file limit to 5MB
  options(shiny.maxRequestSize = 5*1024^2)
  
  # read uploaded data file
  theData <- reactive({
    infile <- input$datafile
    if(is.null(infile))
      return(NULL)
    d <- read.csv(infile$datapath)
  })
  
  # The ROC curve plot
  output$roc <- renderPlot({  plotROC( tab = theData() )})
  
  # The confusion Matrix visualization
  output$confPlot <- renderPlot({
    fourfoldplot(confMatrix(crit = input$crit, tab = theData()),
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 margin = 1,
                 main = "Confusion Matrix")
  })
  
  # The evaluation metrics
  output$metrics <- renderTable({
    
    evalMetrics(df = theData(), crit = input$crit)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
