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
#' @param renderFunc A reactive output function (e.g., [shiny::renderPlot], [shiny::renderText], [shiny::renderUI], etc).
#' @param expr An expression that generates given output expected by `renderFunc`.
#' @param ... Other arguments passed along to `renderFunc`.
#' @inheritParams metaObserve
#'
#' @seealso [metaExpr()]
#' @export
metaRender <- function(renderFunc, expr, ..., env = parent.frame(), quoted = FALSE,
                       localize = "auto", bindToReturn = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Even though expr itself is quoted, wrapExpr will effectively unquote it by
  # interpolating it into the `metaExpr()` call, thus quoted = FALSE.
  expr <- wrapExpr(shinymeta::metaExpr, expr, env, quoted = FALSE, localize = localize, bindToReturn = bindToReturn)

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

  normal <- renderFunc(expr = expr, ..., env = env, quoted = quoted)
  meta <- function() {
    shiny::withReactiveDomain(domain, {
      rlang::eval_tidy(expr, env = env)
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
