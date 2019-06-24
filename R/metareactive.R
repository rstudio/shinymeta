.globals <- new.env(parent = emptyenv())
.globals$dynamicVars <- list()

#' Create a meta-reactive expression
#'
#' Create a [reactive()] that, when invoked with meta-mode activated
#' (i.e. called within [expandCode()] or [withMetaMode()]), returns a
#' code expression (instead of evaluating that expression and returning the value).
#'
#' @details If you wish to capture specific code inside of `expr` (e.g. ignore code
#' that has no meaning outside shiny, like [req()]), use `metaReactive2()` in combination
#' with `metaExpr()`.
#'
#' @inheritParams shiny::reactive
#' @inheritParams metaExpr
#' @export
#' @seealso [metaExpr()]
#' @examples
#'
#' options(shiny.suppressMissingContextError = TRUE)
#'
#' input <- list(x = 1)
#'
#' y <- metaReactive({
#'   req(input$x)
#'   a <- !!input$x + 1
#'   b <- a + 1
#'   c + 1
#' })
#'
#' withMetaMode(y())
#' expandCode(y <- !!y())
#'
#' y <- metaReactive2({
#'   req(input$x)
#'
#'   metaExpr({
#'     a <- !!input$x + 1
#'     b <- a + 1
#'     c + 1
#'   })
#' }, bindToReturn = TRUE)
#'
#' expandCode(y <- !!y())
#'
metaReactive <- function(expr, env = parent.frame(), quoted = FALSE,
                         label = NULL, domain = shiny::getDefaultReactiveDomain(),
                         localize = "auto", bindToReturn = FALSE) {

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

#' Evaluate an expression with meta mode activated
#'
#' @param expr an expression.
#' @param mode whether or not to evaluate expression in meta mode.
#'
#' @seealso [expandCode()]
#' @export
withMetaMode <- function(expr, mode = TRUE) {
  origVal <- metaMode()
  if (!identical(origVal, mode)) {
    metaMode(mode)
    on.exit(metaMode(!mode), add = TRUE)
  }

  if (!getOption("shiny.allowoutputreads", FALSE)) {
    op <- options(shiny.allowoutputreads = TRUE)
    on.exit(options(op), add = TRUE)
  }



  if (mode) {
    expr <- prefix_class(expr, "shinyMetaExpr")
  }

  expr
}

#' Mark an expression as a meta-expression
#'
#'
#'
#' @param expr An expression (quoted or unquoted).
#' @param env An environment.
#' @param quote Is the expression quoted? This is useful when you want to use an expression
#' that is stored in a variable; to do so, it must be quoted with [`quote()`].
#' @param localize Whether or not to wrap the returned expression in [`local()`].
#' The default, \code{"auto"}, only wraps expressions with a top-level [`return()`]
#' statement (i.e., return statements in anonymized functions are ignored).
#' @param bindToReturn For non-`localize`d expressions, should an assignment
#' of a meta expression be applied to the _last child_ of the top-level `\{` call?
#'
#' @seealso [metaReactive2()], [metaObserve2()], [metaRender2()]
#' @export
metaExpr <- function(expr, env = parent.frame(), quoted = FALSE, localize = "auto", bindToReturn = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  metaExpr_(expr, env = env, quoted = quoted, localize = localize, bindToReturn = bindToReturn)
}

metaExpr_ <- function(expr, env = parent.frame(), quoted = FALSE, localize = "auto", bindToReturn = FALSE,
  topLevelDynVars = TRUE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  if (!metaMode()) {
    expr <- expandExpr(expr, list(), env)
    return(rlang::eval_tidy(expr, env = env))
  }

  expr <- comment_flags(expr)
  expr <- expandExpr(expr, if (topLevelDynVars) .globals$dynamicVars, env)
  expr <- strip_outer_brace(expr)

  # Note that bindToReturn won't make sense for a localized call,
  # so determine we need local scope first, then add a special class
  # (we don't yet have the name for binding the return value)
  expr <- add_local_scope(expr, localize)

  expr <- bind_to_return(expr)

  # TODO:
  # 1. let user opt-out of comment elevation?
  # 2. Why does this clobber the bindToReturn class?
  # 3. This is related to bindToReturn, can we exploit that relationship?
  expr <- elevate_comments(expr)

  # flag the call so that we know to bind next time we see this call
  # inside an assign call, we should modify it
  if (bindToReturn && rlang::is_call(expr, "{")) {
    expr <- prefix_class(expr, "bindToReturn")
  }

  expr
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

#' Expand meta primitives into user code
#'
#' This function provides the main entry point for generating user code
#' via meta-components (e.g., [metaReactive()], [metaObserve()], [metaRender()], etc).
#' It's similar to [withMetaMode()], but instead, quotes the `expr`, which allows you
#' to generate code from multiple meta-components via quasiquotation (e.g. [rlang::!!]).
#' When producing code from multiple meta-components, you may find that code produced from one
#' meta-component overlaps (i.e., repeats redundant computation) with another meta-component.
#' In that case, it's desirable to assign the return value of a meta-component to a variable, and
#' use that variable (i.e., symbol) in downstream code generated from other meta-components. This
#' can be done via the `patchCalls` argument which can replace the return value of
#' a meta-component with a relevant variable name.
#'
#' @inheritParams metaExpr
#' @param patchCalls a named list of quoted symbols. The names of the list
#' should match name(s) bound to relevant meta-component(s) found in `expr`
#' (e.g. `petal_width` in the example below). The quoted symbol(s) should
#' match variable name(s) representing the return value of the meta-component(s).
#'
#' @export
#' @seealso [withMetaMode()]
#' @examples
#'
#' options(shiny.suppressMissingContextError = TRUE)
#'
#' petal_width <- metaReactive({
#'   iris$Petal.Width
#' })
#'
#' mean_pw <- metaReactive({
#'   mean(!!petal_width())
#' })
#'
#' expandCode(
#'   {
#'     pw <- !!petal_width()
#'     !!mean_pw()
#'   },
#'   patchCalls = list(
#'     petal_width = quote(pw)
#'   )
#' )
#'
expandCode <- function(expr, env = parent.frame(), quoted = FALSE, patchCalls = list()) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  withMetaMode(
    withDynamicScope(
      metaExpr_(expr, env = env, quoted = quoted, localize = FALSE,
        bindToReturn = FALSE, topLevelDynVars = FALSE),
      .list = lapply(patchCalls, constf)
    )
  )
}

prefix_class <- function (x, y) {
  # Can't set attributes on a symbol, but that's alright because
  # we don't need to flag or compute on symbols
  if (is.symbol(x) || !is.language(x)) return(x)
  oldClass(x) <- unique(c(y, oldClass(x)))
  x
}

remove_class <- function(x, y) {
  if (is.symbol(x) || !is.language(x)) return(x)
  oldClass(x) <- setdiff(oldClass(x), y)
  x
}

quotedList <- function(...) {
  enquote(...)
}
