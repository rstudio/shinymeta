library(shiny)
library(shinymeta)

selectColumnUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    varSelectInput(ns("col"), label, NULL),
    textOutput(ns("average"))
  )
}

selectColumn <- function(input, output, session, df) {
  observeEvent(df(), {
    updateVarSelectInput(session, "col", data = df())
  })

  values <- metaReactive2({
    req(input$col)
    metaExpr(`$`(!!df(), !!input$col))
  })

  avg <- metaReactive({
    round(mean(!!values()), 1)
  })

  output$average <- metaRender(renderText, {
    paste("Average of", !!as.character(input$col), "is", !!avg())
  })

  list(
    values = values,
    average = output$average
  )
}

ui <- fluidPage(
  fluidRow(
    column(3, selectColumnUI("x", "x var")),
    column(3, selectColumnUI("y", "y var"))
  ),
  outputCodeButton(plotOutput("plot"))
)

server <- function(input, output, session) {
  dataset <- metaReactive({mtcars})

  x <- callModule(selectColumn, "x", dataset)
  y <- callModule(selectColumn, "y", dataset)

  df_plot <- metaReactive({
    "# Combine x and y into data frame for plotting"
    data.frame(x = !!x$values(), y = !!y$values())
  })

  output$plot <- metaRender(renderPlot, {
    plot(!!df_plot())
  })

  observeEvent(input$plot_output_code, {
    displayCodeModal(expandChain(
      quote(library(ggplot2)),
      output$plot(),
      x$average(),
      y$average()
    ))
  })
}

shinyApp(ui, server)
