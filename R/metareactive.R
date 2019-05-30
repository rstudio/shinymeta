.globals <- new.env(parent = emptyenv())
.globals$dynamicVars <- list()

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
metaReactive <- function(expr, env = parent.frame(), quoted = FALSE, label = NULL, domain = shiny::getDefaultReactiveDomain()) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Need to wrap expr with shinymeta:::metaExpr, but can't use rlang/!! to do
  # so, because we want to keep any `!!` contained in expr intact (i.e. too
  # early to perform expansion of expr here).
  expr <- wrapExpr(shinymeta::metaExpr, expr, env)

  metaReactiveImpl(expr = expr, env = env, label = label, domain = domain)
}


#' @export
metaReactive2 <- function(expr, env = parent.frame(), quoted = FALSE,
  label = NULL, domain = shiny::getDefaultReactiveDomain()) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  metaReactiveImpl(expr = expr, env = env, label = label, domain = domain)
}

metaReactiveImpl <- function(expr, env, label, domain) {
  force(expr)
  force(env)
  force(label)
  force(domain)

  r_meta <- reactiveWithInputs({
    rlang::eval_tidy(expr, NULL, env)
  }, domain = domain)

  r_normal <- shiny::reactive(expr, env = env, quoted = TRUE, label = label, domain = domain)

  function() {
    if (metaMode()) {
      # r_meta cache varies by dynamicVars
      r_meta(.globals$dynamicVars)
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
  expr <- wrapExpr(shinymeta::metaExpr, expr, env)

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

  prefix_class(expr, if (mode) "shinyMetaExpr" else "shinyMetaValue")
}

#' @export
metaExpr <- function(x, env = parent.frame()) {
  x <- substitute(x)
  x <- expandExpr(x, .globals$dynamicVars, env)
  x <- rlang::new_quosure(x, env)

  if (metaMode())
    rlang::quo_get_expr(x)
  else
    rlang::eval_tidy(x)
}

withDynamicScope <- function(expr, ..., .list = list(...)) {
  if (length(.list) > 0) {
    if (is.null(names(.list)) || !all(nzchar(names(.list)))) {
      stop("withDynamicScope invoked with unnamed vars; all vars must be named")
    }

    oldVars <- .globals$dynamicVars
    .globals$dynamicVars <- c(oldVars[setdiff(names(oldVars), names(.list))], .list)
    on.exit(.globals$dynamicVars <- oldVars)
  }

  expr
  # TODO use promise domain
}

#' @export
expandCode <- function(expr, patchCalls = list(), indent = 0) {
  quosure <- withMetaMode(
    withDynamicScope(
      {
        rlang::enquo(expr)
      },
      .list = lapply(patchCalls, constf)
    )
  )

  expr <- rlang::quo_get_expr(
    remove_class(quosure, "shinyMetaExpr")
  )

  prefix_class(expr, "shinyMetaExpr")
}

prefix_class <- function (x, y) {
  oldClass(x) <- unique(c(y, oldClass(x)))
  x
}

remove_class <- function(x, y) {
  oldClass(x) <- setdiff(oldClass(x), y)
  x
}

quotedList <- function(...) {
  enquote(...)
}
