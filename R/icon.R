#' Overlay an icon on a shiny output
#'
#' Intended for overlaying an icon over a shiny output, that when clicked,
#' displays or downloads code for reproducing that output. The icon
#' works similar to an [shiny::actionButton()], where an input value,
#' tied to the icon, increases by one each time it is pressed. The name
#' of that input is a function of `outputObj`'s `outputId`:
#' `input$OUTPUTID_shinymeta_icon`.
#'
#' @param outputObj A shiny output.
#' @param heading A shiny [tags] object for placing in the header panel.
#'
#' @export
#' @seealso [displayCodeModal]
#' @examples
#'
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     sliderInput("n", label = "Number of samples", min = 10, max = 100, value = 30),
#'     outputCode(plotOutput("p"))
#'   )
#'   server <- function(input, output) {
#'     output$p <- metaRender(renderPlot, {
#'       plot(sample(!!input$n))
#'     })
#'     observeEvent(input$p_shinymeta_icon, {
#'       code <- expandObjects(output$p)
#'       displayCodeModal(code)
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#'
outputCode <- function(outputObj, heading = tags$span(style = "color: #707070", "Show code", icon("code"))) {

  if (!inherits(outputObj, c("shiny.tag", "shiny.tag.list"))) {
    stop("outputObj must be a shiny.tag or shiny.tag.list object")
  }

  shiny::addResourcePath(
    "shinymeta-icon",
    system.file(package = "shinymeta", "lib", "icon")
  )

  div(
    class = "panel panel-default",
    tags$head(
      tags$script(src = "shinymeta-icon/shinymeta-icon.js"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "shinymeta-icon/shinymeta-icon.css"
      )
    ),
    div(
      class = "panel-heading shinymeta-icon",
      a(heading)
    ),
    div(
      class = "panel-body",
      outputObj
    )
  )
}
