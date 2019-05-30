#'@export
metaRender <- function(renderFunc, expr, ..., env = parent.frame(), quoted = FALSE) {
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

  expr <- wrapExpr(shinymeta::metaExpr, expr, env)

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
