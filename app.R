library(shiny)
library(PhysicalActivity)

ui <- fluidPage(
  titlePanel("App for Actigraph Plot and the Summary of Daily Wear Time"),
  sidebarLayout(
    sidebarPanel(
      h4("Uploading File"),
      fileInput('file1', 'Choose AGD File'),
      h4("Download Actigraph Plot"),
      downloadButton('downloadPlot', 'Download Actigraph Plot'),
      h4("Download Table"),
      downloadButton('downloadTable', 'Download Table')
    ),
    mainPanel(
      h3("Data Format"),
      textOutput('descriptionData'),
      h3("Actigraph Plot"),
      plotOutput('actigraphPlot'),
      h3("Summary Table of Daily Wear Time"),
      textOutput('descriptionTable'),
      tableOutput('sumTable')
    )
  )
)

server <- function(input, output) {
  plotInput <- function(){    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dat <- readActigraph(inFile$datapath)
    dd.mark <- wearingMarking(dat, perMinuteCts=1, cts='vm')       
    sumdat <- summaryData(dd.mark)
    plotData(data=dd.mark, cts = "vm", summary=sumdat)
  }
  
  tableInput <- function(){
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dat <- readActigraph(inFile$datapath)
    dd.mark <- wearingMarking(dat, perMinuteCts=1, cts='vm')       
    sumdat <- summaryData(dd.mark)
    sumtable <- data.frame(sumdat$dayInfo)
    return(sumtable)
  }

  output$descriptionData <- renderText({
    "The data file must be an AGD file."
  })
  
  output$actigraphPlot <- renderPlot({
    print(plotInput())
  })
  
  output$descriptionTable <- renderText({
    "The summary of wear time and valid days. For more detailed summary, use the R package PhysicalActivity."
  })
  
  output$sumTable <- renderTable({
    tableInput()
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
      write.csv(tableInput(), file)
    })
  
}

shinyApp(ui = ui, server = server)
