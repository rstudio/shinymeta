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

#' Knitr S3 methods
#'
#' This S3 method allows [metaExpr()]s to print themselves in
#' knitr/rmarkdown documents.
#'
#' @param x Object to knit_print
#' @param ... Additional knit_print arguments
#' @export
knit_print.shinyMetaExpr <- function(x, ...) {
  deparseCode(x)
}
