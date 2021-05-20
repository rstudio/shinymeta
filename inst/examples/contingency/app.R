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
        ),
        selected = "happy"
      ),
      conditionalPanel(
        "input.data == 'custom'",
        # TODO: option to upload pre-aggregated counts?!
        fileInput("data_file", "Upload dataset")
      ),
      uiOutput("vars"),
      checkboxInput("na.rm", "Exclude missing values?", value = TRUE),
      downloadButton("full_code", "Full report", icon = icon("code"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          div(
            plotlyOutput("plot"),
            downloadButton("plot_code", "", icon = icon("code"))
          )
        ),

        tabPanel(
          "Model",
          div(
            checkboxInput("simulate", "Compute p-values by Monte Carlo simulation?"),
            verbatimTextOutput("model"),
            downloadButton("model_code", "", icon = icon("code"))
          )
        ),
        tabPanel(
          "Counts",
          div(tableOutput("table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  getData <- metaReactive({
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

  getVars <- metaReactive({
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
    metaExpr({
      ..(getData()) %>%
        count(!!..(input$xvar), !!..(input$yvar))
    })
  })

  counts_wide <- metaReactive2({
    req(counts_long())

    metaExpr({
      ..(counts_long()) %>%
        tidyr::pivot_wider(
          names_from = !!..(input$xvar),
          values_from = n,
          values_fill = list(n = 0)
        )
    })
  })

  counts_raw <- metaReactive2({
    req(counts_wide())

    metaExpr({
      ..(counts_wide()) %>%
        select(-1) %>%
        as.matrix()
    })
  })

  output$table <- metaRender2(renderTable, {
    validate(need(counts_wide(), "Choose some variables"))
    metaExpr({
      ..(counts_wide())
    })
  })

  output$plot <- metaRender2(renderPlotly, {
    validate(
      need(getData(), "Choose a dataset"),
      need(input$yvar, "Choose a response"),
      need(input$xvar, "Choose a predictor")
    )

    metaExpr({
      gg_plot <- ggplot(..(getData())) +
        geom_mosaic(
          aes(
            x = product(!!..(input$xvar)),
            fill = !!..(input$yvar)
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

  output$model <- metaRender2(renderPrint, {
    validate(need(counts_raw(), "Choose some variables"))

    metaExpr({
      chisq.test(..(counts_raw()), simulate.p.value = isTRUE(..(input$simulate)))
    })
  })

  ec <- newExpansionContext()
  ec$substituteMetaReactive(getData, function() {
    metaExpr({readRDS("data.rds")})
  })

  output$plot_code <- downloadHandler(
    "plot.zip",
    content = function(out) {
      saveRDS(getData(), "data.rds")
      on.exit(unlink("data.rds"), add = TRUE)

      code <- expandChain(
        quote({
          library(plotly)
          library(dplyr)
          library(ggmosaic)
        }),
        output$plot(),
        .expansionContext = ec
      )

      buildRmdBundle(
        "plot.Rmd", out,
        vars = list(code = code),
        include_files = c("data.rds")
      )
    }
  )


  output$model_code <- downloadHandler(
    "model.zip",
    content = function(out) {
      saveRDS(getData(), "data.rds")
      on.exit(unlink("data.rds"), add = TRUE)

      code <- expandChain(
        quote({
          library(plotly)
          library(dplyr)
          library(ggmosaic)
        }),
        output$model(),
        .expansionContext = ec
      )

      buildRmdBundle(
        "model.Rmd", out,
        vars = list(
          code = code,
          xvar = deparse(input$xvar),
          yvar = deparse(input$yvar)
        ),
        include_files = "data.rds"
      )
    }
  )

  output$full_code <- downloadHandler(
    "full.zip",
    content = function(out) {

      saveRDS(getData(), "data.rds")
      on.exit(unlink("data.rds"), add = TRUE)

      code <- expandChain(
        quote({
          library(plotly)
          library(dplyr)
          library(ggmosaic)
        }),
        output$plot(),
        output$model(),
        .expansionContext = ec
      )

      buildRmdBundle(
        "full.Rmd", out,
        vars = list(code = code),
        include_files = "data.rds"
      )
    }
  )

}

shinyApp(ui, server)
