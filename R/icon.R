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
#' @param icon_ A shiny [icon()].
#' @param color A CSS color.
#'
#' @export
#' @seealso [displayCode]
#' @examples
#'
#' library(shiny)
#' ui <- fluidPage(
#'   sliderInput("n", label = "Number of samples", min = 10, max = 100, value = 30),
#'   metaIcon(plotOutput("p"))
#' )
#' server <- function(input, output) {
#'   output$p <- metaRender(renderPlot, {
#'     plot(sample(!!input$n))
#'   })
#'   observeEvent(input$p_shinymeta_icon, {
#'     code <- expandObjects(output$p)
#'     displayEditor(code)
#'   })
#' }
#' shinyApp(ui, server)
#'
metaIcon <- function(outputObj,
                     icon_ = tags$span(style = "color: #707070", icon("code")),
                     tooltip = "Get Code",
                     tooltip_placement = c("bottom", "top", "right", "left")) {

  # TODO: controlable icon positioning

  if (!inherits(outputObj, c("shiny.tag", "shiny.tag.list"))) {
    stop("outputObj must be a shiny.tag or shiny.tag.list object")
  }

  shiny::addResourcePath(
    "shinymeta-icon",
    system.file(package = "shinymeta", "lib", "icon")
  )

  div(
    class = "shinymeta-icon-container",
    tags$head(
      # https://getbootstrap.com/docs/4.0/components/tooltips/
      if (length(tooltip)) tags$script("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); });"),
      tags$script(src = "shinymeta-icon/shinymeta-icon.js"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "shinymeta-icon/shinymeta-icon.css"
      )
    ),
    div(
      class = "shinymeta-icon",
      `data-toggle` = "tooltip",
      `data-placement` = match.arg(tooltip_placement, tooltip_placement),
      title = tooltip,
      icon_
    ),
    outputObj
  )
}
