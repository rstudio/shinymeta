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

  if (is.symbol(expr)) {
    return(expr)
  } else if (is.call(expr)) {
    for (i in seq_len(length(expr) - 1)) {
      expr[[i + 1]] <- replaceInExpr(expr[[i + 1]], target, replacement)
    }
    return(expr)
  }
}
