#' @export
metaObserve <- function(expr, env = parent.frame(), quoted = FALSE,
  label = NULL, domain = getDefaultReactiveDomain()) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  # Need to wrap expr with shinymeta:::metaExpr, but can't use rlang/!! to do
  # so, because we want to keep any `!!` contained in expr intact (i.e. too
  # early to perform expansion of expr here).
  expr <- wrapExpr(shinymeta::metaExpr, expr, env)

  metaObserveImpl(expr = expr, env = env, label = label, domain = domain)
}


#' @export
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

  r_meta <- reactiveWithInputs({
    rlang::eval_tidy(expr, NULL, env)
  }, domain = domain)

  o_normal <- observe(expr, env = env, quoted = TRUE, label = label, domain = domain)

  structure(
    function() {
      if (metaMode()) {
        # r_meta cache varies by dynamicVars
        r_meta(.globals$dynamicVars)
      } else {
        stop("Can't do that") # TODO: better msg
      }
    },
    observer_impl = o_normal,
    class = c("shinymeta_observer", "function")
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
