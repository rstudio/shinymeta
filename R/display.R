#' Display a shinyAce code editor via shiny modal
#'
#' Show a `shinyAce::aceEditor()` in a `shiny::modalDialog()`.
#'
#' @param code Either a language object or a character string.
#' @param clip An [icon()] `name` that a user can press to copy `code` to the clipboard.
#' If you wish to not have an icon, specify `clip = NULL`.
#' @param session a shiny session object (the default should almost always be used).
#' @inheritParams shiny::modalDialog
#' @param ... arguments passed along to `shinyAce::aceEditor()`
#' @return nothing. Call this function for its side effects.
#' @export
#' @seealso [outputCodeButton]
#' @examples
#'
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     sliderInput("n", label = "Number of samples", min = 10, max = 100, value = 30),
#'     actionButton("code", icon("code")),
#'     plotOutput("p")
#'   )
#'   server <- function(input, output) {
#'     output$p <- metaRender(renderPlot, {
#'       plot(sample(..(input$n)))
#'     })
#'     observeEvent(input$code, {
#'       code <- expandChain(output$p())
#'       displayCodeModal(code)
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#'
displayCodeModal <- function(code, title = NULL, clip = "clipboard",
                          footer = shiny::modalButton("Dismiss"), size = c("m", "s", "l"),
                          easyClose = TRUE, fade = TRUE,
                          session = shiny::getDefaultReactiveDomain(), ...) {

  if (system.file(package = "shinyAce") == "") {
    stop("Please install.packages('shinyAce') and try again.")
  }

  if (length(clip) && system.file(package = "clipr") == "") {
    stop("Please install.packages('clipr') and try again.")
  }

  if (is.language(code)) {
    code <- formatCode(code)
  }

  if (!is.character(code)) {
    stop("code must be either a language object or a character string")
  }

  id <- getFromNamespace("createUniqueId", "shiny")(10)

  shiny::observeEvent(session$rootScope()$input[[paste0(id, "-copy")]], {
    clipr::write_clip(code)
  })

  shiny::showModal(
    shiny::modalDialog(
      title = title,
      size = match.arg(size, size),
      easyClose = easyClose,
      fade = fade,
      shinyAce::aceEditor(
        outputId = paste0(id, "-editor"),
        value = paste(code, collapse = "\n"),
        mode = "r",
        readOnly = TRUE,
        ...
      ),
      footer = shiny::tagList(
        if (length(clip)) shiny::actionButton(paste0(id, "-copy"), shiny::icon(clip)),
        footer
      )
    )
  )
}
