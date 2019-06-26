library(shiny)
library(shinymeta)
library(dplyr)

ui <- fluidPage(
  selectInput("dataset", "Dataset", c("iris", "pressure")),
  numericInput("n", "n", 5),
  downloadButton("download_report", "Download report"),
  downloadButton("download_script", "Download script"),
  downloadButton("download_script_bundle", "Download script + output"),
  verbatimTextOutput("code"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  df <- metaReactive(
    get(!!input$dataset, "package:datasets")
  )

  filtered <- metaReactive(
    !!df() %>% head(!!input$n)
  )

  filtered2 <- metaReactive({
    "# a comment inside metaReactive()"
    !!df() %>% tail(!!input$n)
  })

  output$plot <- metaRender(renderPlot, {
    "# This is a helpful comment"
    plot(!!filtered())
  })

  obs <- metaObserve({
    "# Print filtered data"
    print(!!filtered2())
  })

  code <- reactive({
    expandObjects(
      "# Retrieve data",
      df,
      top = filtered,
      bottom = filtered2,
      obs,
      output$plot,
      .pkgs = "magrittr"
    )
  })

  output$code <- renderPrint(code())

  output$download_report <- downloadHandler("report.zip",
    content = function(out) {
      buildRmdBundle("report.Rmd", out, vars = list(code = code()))
    }
  )

  output$download_script_bundle <- downloadHandler("report.zip",
    content = function(out) {
      buildScriptBundle(code(), out, render_args = list(output_format = "html_document"))
    }
  )

  output$download_script <- downloadHandler("script.R",
    content = function(out) {
      writeLines(code(), out)
    }
  )
}

shinyApp(ui, server)
