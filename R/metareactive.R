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
metaReactive <- function(expr, label = NULL, domain = getDefaultReactiveDomain()) {

  #expr <- rlang::enquo(expr)
  #env <- rlang::quo_get_env(expr)
  expr <- substitute(expr)
  env <- parent.frame()

  this_env <- environment()

  # This is definitely wrong, metaExpr may not be visible
  expr <- rlang::expr(shinymeta:::metaExpr(!!expr))

  r_meta <- reactive(expr, env = env, quoted = TRUE, domain = domain)
  r_normal <- reactive(expr, env = env, quoted = TRUE, domain = domain)

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

metaExpr <- function(x) {
  dynvars <- as.list(dynamicVars)
  env <- environment()
  x <- rlang::eval_tidy(quote(rlang::enquo(x)), dynvars, env)
  # x <- rlang::enquo(x)
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
