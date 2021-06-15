#' Overlay an icon on a shiny output
#'
#' Intended for overlaying a button over a shiny output, that when clicked,
#' displays code for reproducing that output. The button is
#' similar to an [shiny::actionButton()], but instead of providing an `inputId`,
#' the id is determined by the id of the `outputObj`. The name
#' of that input is a function of `outputObj`'s `outputId`:
#' `input$OUTPUTID_output_code`.
#'
#' @param outputObj A shiny output container (e.g., [shiny::plotOutput], [shiny::textOutput], etc)
#' @inheritParams shiny::actionButton
#' @return the `outputObj` wrapped in a card-like HTML container.
#' @export
#' @seealso [displayCodeModal]
#' @examples
#'
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     sliderInput("n", label = "Number of samples", min = 10, max = 100, value = 30),
#'     outputCodeButton(plotOutput("p"))
#'   )
#'   server <- function(input, output) {
#'     output$p <- metaRender(renderPlot, {
#'       plot(sample(..(input$n)))
#'     })
#'     observeEvent(input$p_output_code, {
#'       code <- expandChain(output$p())
#'       displayCodeModal(code)
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#'
outputCodeButton <- function(outputObj, label = "Show code", icon = shiny::icon("code"), width = NULL, ...) {

  if (!inherits(outputObj, c("shiny.tag", "shiny.tag.list"))) {
    stop("outputObj must be a shiny.tag or shiny.tag.list object")
  }

  div(
    class = "shinymeta-output-code panel panel-default card",
    htmltools::htmlDependency(
      name = "shinymeta-output-code",
      version = utils::packageVersion("shinymeta"),
      src = "lib/output-code",
      package = "shinymeta",
      script = "output-code.js"
    ),
    div(
      class = "panel-heading card-header",
      # Basically the same as a actionButton(), but there doesn't seem to
      # be a foolproof way to get the outputId given the outputObj, so
      # we won't know the id of this button ahead of time
      tags$button(
        style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
        type = "button",
        class = "btn btn-default",
        list(icon, label),
        ...
      )
    ),
    div(
      class = "panel-body card-body",
      outputObj
    )
  )
}
