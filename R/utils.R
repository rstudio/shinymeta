#' @export
constf <- function(value) {
  force(value)
  function() {
    value
  }
}

#' @export
replaceInExpr <- function(expr, target, replacement) {
  if (identical(expr, target)) {
    return(replacement)
  }

  if (is.call(expr)) {
    for (i in seq_len(length(expr) - 1)) {
      expr[[i + 1]] <- replaceInExpr(expr[[i + 1]], target, replacement)
    }
  }

  expr
}

wrapExpr <- function(namespace, func, ..., private = FALSE) {
  namespace <- substitute(namespace)
  func <- substitute(func)

  as.call(list(
    as.call(list(
      if (private) quote(`:::`) else quote(`::`),
      namespace,
      func
    )),
    ...
  ))
}

# Takes an expr that contains !!, and expands the expr so that all !! are
# resolved. This is similar to rlang::quo except that 1) expr is already
# quoted, and 2) you can specify the data/env from which !! should be
# resolved.
expandExpr <- function(expr, data, env) {
  wrappedExpr <- wrapExpr(rlang, quo, expr)
  rlang::quo_get_expr(eval(wrappedExpr, data, env))
}

# local({
#   a <- quote(one)
#   b <- quote(three)
#   env <- environment()
#   expandExpr(quote(!!a + !!b), list(a = quote(two)), env)
# })
