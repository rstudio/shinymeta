#' Create a meta-reactive output
#'
#' Create a meta-reactive output that, when invoked with meta-mode activated
#' (i.e. called within [expandCode()] or [withMetaMode()]), returns a
#' code expression (instead of evaluating that expression and returning the value).
#'
#' @details If you wish to capture specific code inside of `expr` (e.g. ignore code
#' that has no meaning outside shiny, like [req()]), use `metaRender2()` in combination
#' with `metaExpr()`.
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
  # # Can't use this code, because rlang::exprs(...) causes `!!` to be expanded,
  # # which we don't want. If there was a way to substitute ... without causing
  # # `!!` expansion, that's what we'd want to do.
  # exprs <- rlang::exprs(...)
  # if (length(exprs) == 0) {
  #   stop("render function invoked without any arguments")
  # }
  # exprs_nm <- names(exprs)
  # expr <- if (is.null(exprs_nm)) {
  #   exprs[[1]]
  # } else if ("expr" %in% exprs_nm) {
  #   exprs[["expr"]]
  # } else if ("" %in% exprs_nm) {
  #   exprs[exprs_nm == ""][[1]]
  # }

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Need to wrap expr with shinymeta:::metaExpr, but can't use rlang/!! to do
  # so, because we want to keep any `!!` contained in expr intact (i.e. too
  # early to perform expansion of expr here).
  #
  # Even though expr itself is quoted, wrapExpr will effectively unquote it by
  # interpolating it into the `metaExpr()` call, thus quoted = FALSE.
  expr <- wrapExpr(shinymeta::metaExpr, expr, env, quoted = FALSE, localize = localize, bindToReturn = bindToReturn)

  metaRender2(renderFunc, expr, ..., env = env, quoted = quoted)
}

#' @export
metaRender2 <- function(renderFunc, expr, ..., env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  normal <- renderFunc(expr = expr, ..., env = env, quoted = quoted)
  meta <- function() {
    rlang::eval_tidy(expr, env = env)
  }

  function(...) {
    if (metaMode()) {
      # TODO: Verify that length(list(...)) == 0?
      meta()
    } else {
      if (is.null(formals(normal)))
        normal()
      else
        normal(...)
    }
  }
}
