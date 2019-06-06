library(shiny)
library(shinymeta)
library(plotly)
library(dplyr)
library(ggmosaic)

ui <- fluidPage(
  titlePanel("Contingency table explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data", "Choose a dataset",
        c(
          "Flying Etiquette" = "fly",
          "Happiness" = "happy",
          "Upload your own" = "custom"
        )
      ),
      conditionalPanel(
        "input.data == 'custom'",
        # TODO: option to upload pre-aggregated counts?!
        fileInput("data_file", "Upload dataset")
      ),
      uiOutput("vars"),
      checkboxInput("na.rm", "Exclude missing values?", value = TRUE),
      actionButton("full_code", "Full report", icon = icon("code"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          div(
            plotlyOutput("plot"),
            actionButton("plot_code", "", icon = icon("code"))
          )
        ),

        tabPanel(
          "Model",
          div(
            checkboxInput("simulate", "Compute p-values by Monte Carlo simulation?"),
            verbatimTextOutput("model"),
            actionButton("model_code", "", icon = icon("code"))
          )
        ),
        tabPanel(
          "Counts",
          div(
            tableOutput("table"),
            actionButton("table_code", "", icon = icon("code"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  getData <- reactive({
    d <- switch(
      input$data,
      fly = fly,
      happy = happy,
      custom = if (length(input$data_file)) read.csv(input$data_file$datapath)
    )
    if (is.null(d)) return(NULL)
    d <- mutate_all(d, as.character)
    if (isTRUE(input$na.rm)) na.omit(d) else d
  })

  getVars <- reactive({
    switch(
      input$data,
      fly = list(y = "DoYouRecline", x = "UseElectronicsDuringTakeoff"),
      happy = list(y = "happy", x = "marital"),
      custom = list(y = getData()[1], x = getData()[2])
    )
  })

  output$vars <- renderUI({
    validate(
      need(ncol(getData()) >= 2, "Need a dataset with at least two discrete variables")
    )
    d <- getData()
    div(
      varSelectInput(
        "yvar", "Response variable",
        d, selected = getVars()$y
      ),
      varSelectInput(
        "xvar", "Explanatory variable",
        d, selected = getVars()$x
      )
    )
  })

  counts_long <- metaReactive2({
    req(input$xvar, input$yvar)

    # To increase reproducibility of the code in the
    # report download, we'll provide the result of
    # getData() in the download and have the code
    # import in that R object
    d <- getData()
    metaExpr({
      count(d, !!input$xvar, !!input$yvar)
    })
  })

  counts_wide <- metaReactive2({
    req(counts_long())

    metaExpr({
      counts <- !!counts_long()
      tidyr::pivot_wider(
        counts,
        names_from = !!input$xvar,
        values_from = n,
        values_fill = list(n = 0)
      )
    })
  })

  counts_raw <- metaReactive2({
    req(counts_wide())

    metaExpr({
      countsWide <- !!counts_wide()
      as.matrix(countsWide[, -1])
    })
  })

  output$table <- metaRender2(renderTable, {
    validate(need(counts_wide(), "Choose some variables"))
    metaExpr({
      !!counts_wide()
    })
  })

  output$plot <- metaRender2(renderPlotly, {
    validate(
      need(getData(), "Choose a dataset"),
      need(input$yvar, "Choose a response"),
      need(input$xvar, "Choose a predictor")
    )

    d <- getData()
    metaExpr({
      gg_plot <- ggplot(d) +
        geom_mosaic(
          aes(
            x = product(!!input$xvar),
            fill = !!input$yvar
          )
        ) +
        theme_bw() +
        labs(x = NULL, y = NULL) +
        theme(
          axis.text.x = element_text(
            angle = 45, vjust = 0.95, hjust = 1
          )
        )

      ggplotly(gg_plot) %>%
        config(displayModeBar = FALSE)
    })
  })

  observeEvent(input$plot_code, {

    saveRDS(getData(), "data.rds")
    code <- expandCode({
      d <- readRDS("data.rds")
      !!output$plot()
    })

    build_report(
      "plot.Rmd",
      output_file = "plot.zip",
      vars = list(code = format_tidy_code(code)),
      include_files = c("data.rds")
    )
  })

  output$model <- metaRender2(renderPrint, {
    validate(need(counts_raw(), "Choose some variables"))

    metaExpr({
      counts <- !!counts_raw()
      chisq.test(counts, simulate.p.value = !!isTRUE(input$simulate))
    })
  })

  observeEvent(input$model_code, {

    saveRDS(getData(), "data.rds")
    code <- expandCode({
      d <- readRDS("data.rds")
      !!output$model()
    })

    build_report(
      "model.Rmd",
      output_file = "model.zip",
      vars = list(
        code = format_tidy_code(code),
        xvar = deparse(input$xvar),
        yvar = deparse(input$yvar)
      ),
      include_files = "data.rds"
    )
  })


  observeEvent(input$full_code, {

    saveRDS(getData(), "data.rds")
    code <- expandCode({
      d <- readRDS("data.rds")
      !!output$plot()
      !!output$model()
      !!output$table()
    })

    build_report(
      "full.Rmd",
      output_file = "full.zip",
      vars = list(code = format_tidy_code(code)),
      include_files = "data.rds"
    )
  })

}

shinyApp(ui, server)
