# set working directory to current file location
# rstudioapi::getActiveDocumentContext()
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load utilities
source("init.R")

# user interfact code --------------------
# header
header <- dashboardHeader(title = "ROCme")

# sidebar layout
sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",
                                        # first item - general description
                                        menuItem("Description", tabName = "description", icon = icon("info-circle")),
                                        # second item - upload file
                                        menuItem("Data", tabName = "uploadData", icon = icon("table")),
                                        # third item
                                        menuItem("Analysis", tabName = "analysis", icon = icon("binoculars")),
                                        menuItem("Control", tabName = "Control", icon = icon("sort"),
                                                 menuSubItem(tabName = "decision criterion",
                                                   sliderInput("crit", "Decision Criterion:",
                                                             min = 0.001, max = 0.999,
                                                             value = 0.5, step = 0.025,
                                                             sep = ",",
                                                             animate = TRUE)),
                                                 menuSubItem(tabName = "weight",
                                                   sliderInput("w_fp", "Penalty for False Positive:",
                                                             min = 0.1, max = 0.9,
                                                             value = 0.5, step = 0.1,
                                                             sep = ",",
                                                             animate = TRUE))),
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
  tabItems(
    # description Tab
    tabItem(tabName = "description",
            # description
            shinydashboard::box(status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                uiOutput("inst"))),
    # Upload data
    tabItem(tabName = "uploadData",
            # choose file
            shinydashboard::box(title = "Data Upload",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                fileInput("datafile", "Choose file",
                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
            # Explanations
            shinydashboard::box(status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                uiOutput("format"))),
    # Analysis
    tabItem(tabName = "analysis",
            fluidRow(
            # ROC curve
            shinydashboard::box(title = "Roc Curve",
                                status = "primary",
                                width = 4,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("roc")),
            # Criterion Recommendation
            shinydashboard::box(title = "Recommended Criterion",
                                status = "primary",
                                width = 4,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("reco")),
            # Confusion Matrix
            shinydashboard::box(title = "Confusion Matrix",
                                status = "primary",
                                width = 4,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("confPlot"))
            ),
            # Evluation Metrics
            shinydashboard::box(title = "Evaluation Metrics",
                                status = "primary",
                                width = 3,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                tableOutput("metrics"))
    )
  )
)

# user interface
ui <- dashboardPage(header, sidebar, body)



# code on the server side ---------------------
server <- function(input, output, session) {

  # set file limit to 5MB
  options(shiny.maxRequestSize = 5*1024^2)

  # ROCme description
  output$inst <- renderUI({
    rmarkdown::render(input = "description.Rmd",
                      output_format = html_document(self_contained = TRUE),
                      output_file = 'description.html')
    shiny::includeHTML('description.html')
  })
  
  # format specs/instructions for the uploaded fie
  output$format <- renderUI({
    rmarkdown::render(input = "format.Rmd",
                      output_format = html_document(self_contained = TRUE),
                      output_file = 'format.html')
    shiny::includeHTML('format.html')
  })

  # read uploaded data file
  theData <- reactive({
    infile <- input$datafile
    if(is.null(infile))
      return(NULL)
    d <- read_csv(infile$datapath)
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
  
  # Criterion Recommendation
  output$reco <- renderPlot({
    critReco(tab = theData(), w_fp = input$w_fp)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
