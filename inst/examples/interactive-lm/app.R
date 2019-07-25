# App derived from https://gist.github.com/wch/c4b857d73493e6550cba
library(shiny)
library(shinymeta)
library(shinyAce)
library(dplyr)
library(ggplot2)

# Define the data dataset of interest
data <- mtcars

# User interface
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      varSelectInput("yvar", "Select y", data),
      varSelectInput("xvar", "Select x", data),
      selectInput("degree", "Polynomial degree", c(1, 2, 3, 4))
    ),
    mainPanel(
      outputCodeButton(plotOutput("plot", click = "plot_click"))
    )
  )
)

# This column will track which rows have been excluded
data <- tibble::rownames_to_column(data, var = ".row_ids")

server <- function(input, output) {
  # For storing which row ids have been excluded
  outliers <- reactiveVal(NULL)

  # Toggle points that are clicked
  observeEvent(input$plot_click, {
    # TODO: handle more than one pt at a time
    row_id <- nearPoints(data, input$plot_click)$.row_ids[1]
    if (!length(row_id)) return()

    # If this point is already an outlier, then
    # it's not longer considered an outlier.
    if (row_id %in% outliers()) {
      outliers(setdiff(outliers(), row_id))
    } else {
      outliers(c(row_id, outliers()))
    }
  })

  data_discard <- metaReactive({
    filter(data, .row_ids %in% ..(outliers()))
  })

  data_kept <- metaReactive({
    filter(data, !.row_ids %in% ..(outliers()))
  })

  model_fit <- metaReactive2({
    req(input$degree)

    # just say no to as.formula
    form <- substitute(
      y ~ poly(x, degree = degree),
      list(
        y = input$yvar,
        x = input$xvar,
        degree = as.numeric(input$degree)
      )
    )

    metaExpr(
      lm(form, data = ..(data_kept()))
    )
  })

  data_fitted <- metaReactive({
    modelr::add_predictions(..(data_kept()), ..(model_fit()))
  })

  output$plot <- metaRender(renderPlot, {
    ggplot(..(data_kept()), aes(x = !!..(input$xvar), y = !!..(input$yvar))) +
      geom_point() +
      geom_line(data = ..(data_fitted()), aes(y = pred), color = "gray50") +
      geom_point(data = ..(data_discard()), fill = NA, color = "black", alpha = 0.25) +
      theme_bw(base_size = 14)
  })

  observeEvent(input$plot_output_code, {
    code <- expandChain(
      quote({
        library(ggplot2)
        library(dplyr)
        library(modelr)
        data <- mtcars
        data <- tibble::rownames_to_column(data, var = ".row_ids")
      }),
      output$plot()
    )

    displayCodeModal(
      code = code,
      title = "Code to reproduce data and plot"
    )
  })
}

shinyApp(ui, server)
