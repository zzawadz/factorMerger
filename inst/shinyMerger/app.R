library(shiny)
library(factorMerger)
library(futile.logger)
library(readr)

flog.threshold(DEBUG)

ui <- fluidPage(

  titlePanel("Factor Merger"),
  fluidRow(column(2,
  fileInput("dataFile",label = "Upload CSV"),
  downloadButton("cleanData", "Download Your new data!")),
  column(10,fluidRow(factorMergerUI("merger"),   style = "margin-left:20px"))),

  verbatimTextOutput("functionCall")
)

server <- function(input, output, session) {

  fileInfo = reactive({
    req(input$dataFile)
    flog.debug("[MergerApp] File downloaded.")
    input$dataFile
  })

  dataSet = reactive({
    req(fileInfo())

    ext = tools::file_ext(fileInfo()$name)

    if(ext != "csv")
    {
      stop(paste("This is not a csv! You have uploaded", ext))
      req(NULL)
    }


    dt = read_csv(fileInfo()$datapath)
    flog.debug("[MergerApp] Data loaded.")
    dt
  })

  merger = reactive({

    req(dataSet())
    flog.debug("[MergerApp] Data passed to merger.")
    callModule(factorMergerServer, "merger", session = session, dataSet())
  })
  observe({merger()})

  output$cleanData <- downloadHandler(
    filename = function() {
      paste("mergedFactor", fileInfo()$name, sep = "-")
    },
    content = function(file) {
      req(dataSet())

      dt = dataSet()
      dt = merger()$remap(dt)
      write.csv(dt, file, row.names = FALSE)
    }
  )

  output$functionCall = renderText({
    req(merger())
    paste('library(factorMerger)',merger()$mergerFactorFunctionAsString, sep = "\n")
  })

}

# Run the application
shinyApp(ui = ui, server = server)

