# Creates a nullary function that returns the given value
constf <- function(value) {
  force(value)
  function() {
    value
  }
}

#' Wrap expressions with a function call
#'
#' Creates a function call with the given function, using the quoted expressions
#' as unnamed arguments.
#'
#' @param func Fully-qualified function (i.e. base::head or shiny:::withLocalOptions)
#' @param ... Language objects to pass to the function
#' @noRd
wrapExpr <- function(func, ...) {
  func <- substitute(func)

  as.call(list(
    if (is.call(func)) as.call(func) else as.symbol(func),
    ...
  ))
}

# Takes an expr that contains !!, and expands the expr so that all !! are
# resolved. This is similar to rlang::quo except that 1) expr is already
# quoted, and 2) you can specify the data/env from which !! should be
# resolved. For example,
#
# local({
#   a <- quote(one)
#   b <- quote(three)
#   env <- environment()
#   expandExpr(quote(!!a + !!b), list(a = quote(two)), env)
# })
expandExpr <- function(expr, data, env) {
  wrappedExpr <- wrapExpr(rlang::quo, expr)
  rlang::quo_get_expr(eval(wrappedExpr, data, env))
}


strip_outer_brace <- function(expr) {
  while (rlang::is_call(expr, "{", 1)) {
    expr <- expr[[2]]
  }
  expr
}


reactiveWithInputs <- function(expr, env = parent.frame(), quoted = FALSE, domain = getDefaultReactiveDomain()) {
  map <- fastmap::fastmap()

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  force(env)
  force(quoted)
  force(domain)

  function(...) {
    hash <- isolate(digest::digest(list(...), algo = "sha1"))
    if (!map$has(hash)) {
      map$set(hash, shiny::reactive(expr, env = env, quoted = quoted, domain = domain))
    }
    map$get(hash)()
  }
}
