#' Create a meta-reactive expression
#'
#' Create a reactive that operates in one of two modes, depending on how it is
#' invoked.
#'
#' When invoked normally, it operates just like a normal reactive.
#'
#' When invoked with meta-mode activated (i.e. withMetaMode is on the call
#' stack), instead of executing the code and returning the value, it returns the
#' code expression.
#' @export
metaReactive <- function(expr, env = parent.frame(), quoted = FALSE, label = NULL, domain = getDefaultReactiveDomain()) {

  #expr <- rlang::enquo(expr)
  #env <- rlang::quo_get_env(expr)
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Need to wrap expr with shinymeta:::metaExpr, but can't use rlang/!! to do
  # so, because we want to keep any `!!` contained in expr intact (i.e. too
  # early to perform expansion of expr here).
  expr <- wrapExpr(shinymeta, metaExpr, expr, env, private = TRUE)

  r_meta <- reactive(expr, env = env, quoted = quoted, domain = domain)
  r_normal <- reactive(expr, env = env, quoted = quoted, domain = domain)

  function() {
    if (metaMode()) {
      r_meta()
    } else {
      r_normal()
    }
  }
}


#' @export
metaReactive2 <- function(expr, env = parent.frame(), quoted = FALSE,
  label = NULL, domain = getDefaultReactiveDomain()) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  r_meta <- reactive(expr, env = env, quoted = quoted, domain = domain)
  r_normal <- reactive(expr, env = env, quoted = quoted, domain = domain)

  function() {
    if (metaMode()) {
      r_meta()
    } else {
      r_normal()
    }
  }
}

#' @export
metaAction <- function(expr, env = parent.frame(), quoted = FALSE) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Need to wrap expr with shinymeta:::metaExpr, but can't use rlang/!! to do
  # so, because we want to keep any `!!` contained in expr intact (i.e. too
  # early to perform expansion of expr here).
  expr <- wrapExpr(shinymeta, metaExpr, expr, env, private = TRUE)

  function() {
    rlang::eval_tidy(expr, NULL, env)
  }
}

#' @export
metaMode <- local({
  isMetaMode <- FALSE
  function(value) {
    if (missing(value)) {
      isMetaMode
    } else {
      isMetaMode <<- value
    }
  }
})

#' @export
withMetaMode <- function(expr, mode = TRUE) {
  origVal <- metaMode()
  if (!identical(origVal, mode)) {
    metaMode(mode)
    on.exit(metaMode(!mode))
  }

  force(expr)
}

#' @export
metaExpr <- function(x, env = parent.frame()) {
  dynvars <- as.list(dynamicVars)
  # x <- rlang::eval_tidy(quote(rlang::enquo(x)), dynvars, env)
  x <- substitute(x)
  # if (metaMode()) browser()
  x <- expandExpr(x, dynvars, env)
  x <- rlang::new_quosure(x, env)

  if (metaMode())
    rlang::quo_get_expr(x)
  else
    rlang::eval_tidy(x)
}

dynamicVars <- new.env(parent = emptyenv())

#' @export
withDynamicScope <- function(expr, ...) {
  vars <- list(...)
  mapply(function(key, val) {
    dynamicVars[[key]] <- val
    NULL
  }, names(vars), vars)

  expr

  # TODO undo changes to dynamicVars
  # TODO use promise domain
}
