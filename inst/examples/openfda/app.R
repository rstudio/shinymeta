library(shiny)
library(plotly)
library(dplyr)
source("pca-plot.R")

# TODO:
# (1) save/read outcomes_tidy
# (2) Generate code to recreate pca-outcomes.rds?
outcomes <- readRDS("data/outcomes.rds")
pca_outcomes <- readRDS("data/pca-outcomes.rds")

ndrugs <- length(outcomes)

# from RColorBrewer Set3, but reordered
color_palette <- c(
  "#8DD3C7", "#BC80BD", "#B3DE69", "#FDB462", "#FFFFB3", "#BEBADA",
  "#FB8072", "#80B1D3", "#FCCDE5", "#D9D9D9", "#CCEBC5", "#FFED6F"
)
color_palette <- rep(color_palette, length.out = ndrugs)
plotly_fonts <- list(size = 12, family = "HelveticaNeue")

# tidy-up data
tidy_data <- function(d) {
  dfull <- Map(function(x, y) { x[["drug"]] <- y; x}, d, names(d))

  dat <- bind_rows(dfull)

  overall <- dat %>%
    group_by(term) %>%
    summarise(count = sum(count)) %>%
    mutate(drug = "overall")

  overall %>%
    bind_rows(dat) %>%
    group_by(drug) %>%
    mutate(perc = count / sum(count))
}

outcomes_tidy <- tidy_data(outcomes)

outcomes_tidy <- outcomes_tidy %>%
  mutate(
    term = recode(
      term,
      `1` = "Recovered/resolved",
      `2` = "Recovering/resolving",
      `3` = "Not recovered/not resolved",
      `4` = "Recovered/resolved with sequelae",
      `5` = "Fatal",
      `6` = "Unknown"
    )
  )

outcome_bars <- outcomes_tidy %>%
  filter(drug == "overall") %>%
  plot_ly(
    x = ~perc, y = ~term,
    hoverinfo = "x+name", name = "overall"
  ) %>%
  add_bars(
    text = ~term, textposition = "outside",
    textfont = list(color = "black"), cliponaxis = FALSE,
    marker = list(color = color_palette[1])
  ) %>%
  layout(
    font = plotly_fonts,
    title = "Distribution of outcomes",
    showlegend = FALSE,
    margin = list(l = 10),
    hovermode = "y",
    yaxis = list(title = "", showticklabels = FALSE),
    xaxis = list(title = "", tickformat = "%")
  ) %>%
  config(displayModeBar = FALSE)


ui <- fluidPage(
  titlePanel("Visualizing FDA adverse drug events (of 1000 drugs)"),
  # TODO: add indications as a tabSet?
  fluidRow(
    column(7, plotlyOutput("outcomes")),
    column(5, plotlyOutput("outcome_bars"))
  )
)

server <- function(input, output, session) {

  selected_drugs <- reactiveVal(NULL)

  observeEvent(event_data("plotly_click", source = "pca"), {
    drug <- event_data("plotly_click", source = "pca")$customdata
    if (drug %in% selected_drugs()) {
      selected_drugs(setdiff(selected_drugs(), drug))
    } else {
      selected_drugs(c(selected_drugs(), drug))
    }
    selected_drugs(
      setNames(
        selected_drugs(),
        color_palette[-1][seq_along(selected_drugs())]
      )
    )
  })

  output$outcomes <- renderPlotly({
    plot_pca(
      pca_outcomes, dims = 2, drugs = selected_drugs(),
      base_col = color_palette[1], title = "PCA of outcomes"
    )
  })

  output$outcome_bars <- renderPlotly({

    for (i in seq_along(selected_drugs())) {
      d <- filter(outcomes_tidy, drug %in% selected_drugs()[[i]])
      outcome_bars <- add_bars(
        outcome_bars, x = d[["perc"]], y = d[["term"]],
        name = selected_drugs()[[i]],
        marker = list(color = color_palette[i+1])
      )
    }

    outcome_bars
  })

}

shinyApp(ui, server)
