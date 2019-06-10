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
  verbatimTextOutput("text"),
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

  summarize <- metaAction(summary(!!filtered()))

  output$text <- renderPrint({
    summarize()
  })

  output$plot <- metaRender(renderPlot, {
    "# This is a helpful comment"
    plot(!!filtered())
  })

  obs <- metaObserve({
    print(!!filtered2())
  })

  output$code <- renderPrint({
    expandCode(
      {
        library(dplyr)

        df <- !!df()
        '# a comment inside expandCode()'
        top <- !!filtered()
        bottom <- !!filtered2()
        !!summarize()
        !!output$plot()
        !!obs()
      },
      patchCalls = list(
        df = quote(df),
        filtered = quote(top),
        filtered2 = quote(bottom)
      )
    )
  })

  output$download_report <- downloadHandler("report.zip",
    content = function(out) {
      code <- expandCode(!!output$code())

      build_rmd_bundle("report.Rmd", out, vars = list(code = code))
    }
  )

  output$download_script_bundle <- downloadHandler("report.zip",
    content = function(out) {
      code <- expandCode(!!output$code())

      # build_rmd_bundle("report.Rmd", out, vars = list(code = code))
      build_script_bundle(code, out, render = TRUE, render_args = list(
        output_format = "html_document"
      ))
    }
  )

  output$download_script <- downloadHandler("script.R",
    content = function(out) {
      code <- expandCode(!!output$code())
      writeLines(code, out)
    }
  )
}

shinyApp(ui, server)
