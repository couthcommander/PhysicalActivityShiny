library(shiny)
library(shinyjs)
library(PhysicalActivity)
load('tzopt.Rdata')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("App for Actigraph Plot and the Summary of Daily Wear Time"),
  sidebarLayout(
    sidebarPanel(
      actionButton('go', 'Create Output', style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      h4("Uploading File"),
      fileInput('file1', 'Choose AGD/CSV File'),
      h4("Download Actigraph Plot"),
      downloadButton('downloadPlot', 'Download Actigraph Plot'),
      h4("Download Table"),
      downloadButton('downloadTable', 'Download Table'),
      h4("Options"),
      checkboxInput('delcalc', 'Allow Delivery Day Calculation'),
      tabsetPanel(
        tabPanel("Package options", wellPanel(
          textInput('cts', 'Count variable', 'vm'),
          textInput('ts', 'Timestamp variable', 'TimeStamp'),
          numericInput('vc', 'Cutoff for Valid Day', 600, 1)
        )),
        tabPanel("wearingMarking options", wellPanel(
          numericInput('frame', 'Frame', 90, 1),
          numericInput('permin', 'Counts per minute', 1, 1),
          numericInput('allow', 'Allowance Frame', 2, 1),
          selectInput('tz', 'Timezone', tzopt, selected = "UTC")
        )),
        tabPanel("markDelivery options", wellPanel(
          hidden(div(id = "delopt",
              selectInput('window', 'Window', list('trim','consecutive','valid'), selected = "trim"),
              selectInput('method', 'Method', list('95','mean','sd'), selected = "95"),
              numericInput('thresh', 'Wear Threshold', 300, 1)
          ))
        )),
        type = 'pills'
      ),
      width = 3
    ),
    mainPanel(
      h3("Data Format"),
      textOutput('descriptionData'),
      h3("Actigraph Plot"),
      plotOutput('actigraphPlot'),
      h3("Summary Table of Daily Wear Time"),
      textOutput('descriptionTable'),
      tableOutput('sumTable'),
      tableOutput('deldays')
    )
  )
)

server <- function(input, output) {
  
  v <- reactiveValues(dat = NULL)

  observeEvent(input$delcalc, {
    if(input$delcalc) {
      shinyjs::show('delopt')
    } else {
      shinyjs::hide('delopt')
    }
  })

  observeEvent(input$file1, {
    inFile <- input$file1
    if(!is.null(inFile)) {
      if(inFile$type %in% c('text/csv', 'text/comma-separated-values', 'text/plain')) {
        v$dat <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      } else {
        v$dat <- readActigraph(inFile$datapath)
      }
    } else {
      v$dat <- NULL
    }
  })

  wm <- eventReactive(input$go, {
    if(is.null(v$dat)) {
      return(NULL)
    } else {
      f <- input$frame
      af <- input$allow
      pm <- input$permin
      tz <- input$tz
      wind <- input$window
      meth <- input$method
      wt <- input$thresh
      # set package options
      options(pa.timestamp = input$ts, pa.cts = input$cts, pa.validCut = input$vc)
      # check for bad column names
      if(any(!(c(input$ts, input$cts) %in% names(v$dat)))) return(NULL)
      wearmk <- wearingMarking(v$dat, f, pm, allowanceFrame = af, tz = tz)
      if(input$delcalc) {
        markdv <- markDelivery(wearmk, window = wind, method = meth, wearThreshold = wt)
        sumdat <- summaryData(wearmk, perMinuteCts = pm, delivery = markdv)
      } else {
        sumdat <- summaryData(wearmk, perMinuteCts = pm)
      }
    }
    list(wearmk, sumdat)
  })

  plotInput <- function(){
    res <- wm()
    if(is.null(res)) return(NULL)
    plotData(data=res[[1]], summary=res[[2]])
  }

  tableInput <- function(){
    res <- wm()
    if(is.null(res)) return(NULL)
    sumdat <- res[[2]]
    sumtable <- data.frame(sumdat$dayInfo)
    return(sumtable)
  }

  deliveryTable <- function() {
    res <- wm()
    if(is.null(res)) return(NULL)
    sumdat <- res[[2]]
    if(!('deliveryDays' %in% names(sumdat))) return(NULL)
    data.frame(deliveryDays = names(sumdat$deliveryDays))
  }

  output$descriptionData <- renderText({
    "The data file must be an AGD or CSV file."
  })

  output$actigraphPlot <- renderPlot({
    # print(plotInput())
    plotInput()
  })

  output$descriptionTable <- renderText({
    "The summary of wear time and valid days. For more detailed summary, use the R package PhysicalActivity."
  })

  output$sumTable <- renderTable({
    tableInput()
  })

  output$deldays <- renderTable({
    deliveryTable()
  })

  output$downloadPlot <- downloadHandler(
    filename = function() { paste('plot', Sys.Date(), '.pdf', sep='') },
    content = function(file, width=15, height=6) {
      pdf(file, width=width, height=height)
      plotInput()
      dev.off()
    })

  output$downloadTable <- downloadHandler(
    filename = function() { paste('table', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(tableInput(), file, row.names = FALSE)
    })

}

shinyApp(ui = ui, server = server)
