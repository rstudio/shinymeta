#' @export
print.shinyMetaExpr <- function(x, ...) {
  print(formatCode(x), ...)
}

#' @export
print.shinyMetaString <- function(x, ...) {
  print(deparseCode(x), ...)
}

#' @export
as.character.shinyMetaExpr <- function(x, ...) {
  as.character(deparseCode(x), ...)
}

#' @export
format.shinyMetaExpr <- function(x, ...) {
  format(deparseCode(x), ...)
}

#' @export
knit_print.shinyMetaExpr <- function(x, ...) {
  deparseCode(x)
}
