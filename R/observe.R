#' Create a meta-reactive observer
#'
#' Create a [observe()]r that, when invoked with meta-mode activated
#' (i.e. called within [withMetaMode()] or [expandChain()]), returns a
#' partially evaluated code expression. Outside of meta-mode,
#' `metaObserve()` is equivalent to `observe()`
#' (it fully evaluates the given expression).
#'
#' @details If you wish to capture specific code inside of `expr` (e.g. ignore code
#' that has no meaning outside shiny, like [req()]), use `metaObserve2()` in combination
#' with `metaExpr()`. When using `metaObserve2()`, `expr` must return a `metaExpr()`.
#'
#' @inheritParams shiny::observe
#' @inheritParams metaReactive
#' @inheritParams metaExpr
#' @seealso [metaExpr()], [`..`][shinymeta::dotdot]
#' @export
#' @examples
#'
#' # observers execute 'immediately'
#' x <- 1
#' mo <- metaObserve({
#'   x <<- x + 1
#' })
#' shiny:::flushReact()
#' print(x)
#'
#' # It only makes sense to invoke an meta-observer
#' # if we're in meta-mode (i.e., generating code)
#' expandChain(mo())
#'
#' # Intentionally produces an error
#' \dontrun{mo()}
#'
metaObserve <- function(expr, env = parent.frame(), quoted = FALSE,
  label = NULL, domain = getDefaultReactiveDomain(),
  localize = "auto", bindToReturn = FALSE) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Even though expr itself is quoted, wrapExpr will effectively unquote it by
  # interpolating it into the `metaExpr()` call, thus quoted = FALSE.
  expr <- wrapExpr(shinymeta::metaExpr, expr, env, quoted = FALSE, localize = localize, bindToReturn = bindToReturn)

  metaObserveImpl(expr = expr, env = env, label = label, domain = domain)
}

#' @inheritParams metaObserve
#' @export
#' @rdname metaObserve
metaObserve2 <- function(expr, env = parent.frame(), quoted = FALSE,
  label = NULL, domain = getDefaultReactiveDomain()) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  metaObserveImpl(expr = expr, env = env, label = label, domain = domain)
}

metaObserveImpl <- function(expr, env, label, domain) {
  force(expr)
  force(env)
  force(label)
  force(domain)

  r_meta <- function() {
    shiny::withReactiveDomain(domain, {
      rlang::eval_tidy(expr, NULL, env)
    })
  }

  o_normal <- shiny::observe(expr, env = env, quoted = TRUE, label = label, domain = domain)

  structure(
    function() {
      metaDispatch(
        normal = {
          stop("Meta mode must be activated when calling the function returned by `metaObserve()`: did you mean to call this function inside of `expandChain()`?")
        },
        meta = {
          r_meta()
        }
      )
    },
    observer_impl = o_normal,
    class = c("shinymeta_observer", "shinymeta_object", "function")
  )
}

#' @export
`$.shinymeta_observer` <- function(x, name) {
  obs <- attr(x, "observer_impl", exact = TRUE)
  obs[[name]]
}

#' @export
`[[.shinymeta_observer` <- function(x, name) {
  obs <- attr(x, "observer_impl", exact = TRUE)
  obs[[name]]
}
