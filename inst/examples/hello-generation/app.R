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
  df <- metaReactive({
    get(..(input$dataset), "package:datasets")
  })

  filtered <- metaReactive({
    ..(df()) %>% head(..(input$n))
  })

  output$text <- renderPrint({
    summary(filtered())
  })

  output$plot <- renderPlot({
    plot(filtered())
  })

  output$code <- renderPrint({
    expandChain(filtered())
  })
}

shinyApp(ui, server)
