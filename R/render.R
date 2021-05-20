#' Create a meta-reactive output
#'
#' Create a meta-reactive output that, when invoked with meta-mode activated
#' (i.e. called within [expandChain()] or [withMetaMode()]), returns a
#' code expression (instead of evaluating that expression and returning the value).
#'
#' @details If you wish to capture specific code inside of `expr` (e.g. ignore code
#' that has no meaning outside shiny, like [req()]), use `metaRender2()` in combination
#' with `metaExpr()`. When using `metaRender2()`, `expr` must return a `metaExpr()`.
#'
#' Since package authors are allowed to create their own output rendering functions,
#' creating a meta-counterpart of an output renderer (e.g. `renderPlot()`) needs to be
#' more general than prefixing `meta` to the function name (as with `metaReactive()` and `metaObserve()`).
#' `metaRender()` makes some assumptions about the arguments taken by the render function,
#' assumptions that we believe are true for all existing render functions.
#' If you encounter a render function that doesn't seem to work properly,
#' please let us know by [filing an issue on GitHub](https://github.com/rstudio/shinymeta/issues).
#'
#' @param renderFunc A reactive output function (e.g., [shiny::renderPlot], [shiny::renderText], [shiny::renderUI], etc).
#' @param expr An expression that generates given output expected by `renderFunc`.
#' @param ... Other arguments passed along to `renderFunc`.
#' @inheritParams metaObserve
#'
#' @seealso [metaExpr()], [`..`][shinymeta::dotdot]
#' @export
#' @examples
#'
#' if (interactive()) {
#'   library(shiny)
#'   library(shinymeta)
#'
#'   ui <- fluidPage(
#'     selectInput("var", label = "Choose a variable", choices = names(cars)),
#'     verbatimTextOutput("Summary"),
#'     verbatimTextOutput("code")
#'   )
#'
#'   server <- function(input, output) {
#'     var <- metaReactive({
#'       cars[[..(input$var)]]
#'     })
#'     output$Summary <- metaRender(renderPrint, {
#'       summary(..(var()))
#'     })
#'     output$code <- renderPrint({
#'       expandChain(output$Summary())
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
metaRender <- function(renderFunc, expr, ..., env = parent.frame(),
  quoted = FALSE, localize = "auto", bindToReturn = FALSE) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Even though expr itself is quoted, wrapExpr will effectively unquote it by
  # interpolating it into the `metaExpr()` call, thus quoted = FALSE.
  expr <- wrapExpr(shinymeta::metaExpr, expr, quoted = FALSE,
    localize = localize, bindToReturn = bindToReturn)

  metaRender2(renderFunc, expr, ..., env = env, quoted = quoted)
}

#' @export
#' @rdname metaRender
metaRender2 <- function(renderFunc, expr, ..., env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  domain <- getDefaultReactiveDomain()

  # 3rd party render functions, such as htmlwidgets::shinyRenderWidget() might
  # be using shiny::installExprFunction() + shiny::createRenderFunction() to
  # implement the rendering function, which won't work with the rlang::inject()
  # approach that Shiny 1.6 release suggests. If and when installExprFunction()
  # becomes quosure aware, which it might in
  # https://github.com/rstudio/shiny/pull/3390, we should be able to change this
  # line to use rlang::inject() in order to avoid the env/quoted deprecating
  # warning
  normal <- renderFunc(expr = expr, ..., env = env, quoted = quoted)

  meta <- function() {
    shiny::withReactiveDomain(domain, {
      eval(expr, envir = new.env(parent = env))
    })
  }

  structure(
    function(...) {
      metaDispatch(
        normal = {
          if (is.null(formals(normal)))
            normal()
          else
            normal(...)
        },
        meta = {
          # TODO: Verify that length(list(...)) == 0?
          meta()
        }
      )
    },
    class = c("shinymeta_render", "shinymeta_object", "function")
  )
}
