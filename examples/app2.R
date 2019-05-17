library(shiny)
library(shinymeta)
library(dplyr)

ui <- fluidPage(
  selectInput("dataset", "Dataset", c("iris", "pressure")),
  numericInput("n", "n", 5),
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

  filtered2 <- metaReactive(
    !!df() %>% tail(!!input$n)
  )

  summarize <- metaAction({
    summary(!!filtered())
  })

  output$text <- renderPrint({
    summarize()
  })

  output$plot <- renderPlot({
    plot(filtered())
  })

  output$code <- renderPrint({
    exp <- withDynamicScope(

      df = constf(quote(df)),
      filtered = constf(quote(top)),

      withMetaMode({
        rlang::expr({
          df <- !!df()
          top <- !!filtered()
          bottom <- !!filtered2()
          !!summarize()
        })
      })
    )

    styler::style_text(capture.output(print(exp)))
  })
}

shinyApp(ui, server)
