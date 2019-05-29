#' Print method for shinymeta metaExpression
#'
#' @param x an object of class shinyMetaExpr.
#' @param formatter a function that takes an unevaluated expression
#' and returns a character string.
#' @param ... arguments passed along to [cat()].
#'
#' @export
print.shinyMetaExpr <- function(x, formatter = format_tidy_code, ...) {
  cat(formatter(x), ..., sep = "\n")
}
