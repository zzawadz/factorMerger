factorMergerUI = function(id) {
  ns = NS(id)
  list(
  sliderInput(ns("cutOff"), label = "Cutoff level", min = 0, max = 100, value = 0),
  selectInput(ns("columnName"), label = "Select column", choices = NULL),
  dragulaMultiTableOutput(ns("dragula")))
}

factorMergerServer = function(input, output, session, data)
{
  values = reactiveValues(remap = function(x) x, remapConfig = list(), mergerFactorFunctionAsString = "mergeFactorsInYourData = function(x) x")

  observe({
    classes  = sapply(data, function(x) class(x))

    colNames = colnames(data)
    colNames = colNames[classes %in% c("factor", "character")]

    colNames = colNames[sapply(colNames, function(x) (factor(data[[x]]) %>%
                                                        levels %>%
                                                        length))  < 51]

    updateSelectInput(session, inputId = "columnName", choices = colNames)
  }, priority = 100)


  output$dragula = renderDragulaMultiTable({
    req(input$columnName)
    levels = data[[input$columnName]]
    groups = createGroups(levels, input$cutOff / 100)
    #if(input$columnName == "Group") browser()
    dragulaMultiTable(groups)
  })

  observeEvent(input$dragula, {
    req(input$columnName)

    levels = lapply(input$dragula, unlist)
    names(levels) = NULL

    levels = levels[sapply(levels, length) > 0]

    values$remapConfig[[input$columnName]] = levels

    remapConfig = values$remapConfig

    #if(length(remapConfig) > 0) browser()

    output = capture.output(dput(remapConfig))
    if(length(output) > 1)
    {
      output = paste(output, collapse = "")
    }

    values$mergerFactorFunctionAsString = sprintf("
mergeFactorsInYourData = function(x)
{
  remapConfig = %s
  remapData(data, remapConfig = remapConfig)
}
            ", output)



    values$remap = function(data)
    {
      remapData(data, remapConfig = remapConfig)
    }

  })



  return(values)
}
