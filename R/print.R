#' Print method for shinymeta metaExpression
#'
#' @param x an object of class shinyMetaExpr.
#' @param ... arguments passed along to [formatCode()].
#'
#' @export
#' @keywords internal
print.shinyMetaExpr <- function(x, ...) {
  print(formatCode(x, ...))
}
