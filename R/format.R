#' Deparse and format shinymeta expressions
#'
#' Turn unevaluated shinymeta expressions into (formatted or styled) text.
#'
#' Before any formatting takes place, the unevaluated expression is
#' deparsed into a string via [deparseCode()], which ensures that
#' shinymeta comment strings (i.e., literal strings that appear on their own line,
#' and begin with one or more `#` characters.) are turned into comments and
#' superfluous `\{` are removed. After deparsing, the `formatCode()` function then
#' calls the `formatter` function on the deparsed string to format (aka style) the code string.
#' The default `formatter`, `styleText()`, uses [styler::style_text()] with a couple differences:
#'
#' * Pipe operators (`%>%`) are _always_ followed by a line break.
#' * If the token appearing after a line-break is a comma/operator, the line-break is removed.
#'
#' @param code Either an unevaluated expression or a deparsed code string.
#' @param width The `width.cutoff` to use when [deparse()]-ing the `code` expression.
#' @param formatter a function that accepts deparsed code (a character string)
#' as the first argument.
#' @param ... arguments passed along to the `formatter` function.
#' @return Single-element character vector with formatted code
#' @export
#' @examples
#'
#' options(shiny.suppressMissingContextError = TRUE)
#'
#' x <- metaReactive({
#'   "# Here's a comment"
#'   sample(5) %>% sum()
#' }, varname = "x")
#'
#' code <- expandChain(x())
#'
#' deparseCode(code)
#' formatCode(code)
#' formatCode(code, formatter = styler::style_text)
formatCode <- function(code, width = 500L, formatter = styleText, ...) {
  if (!inherits(code, "shinyMetaDeparsed")) {
    code <- deparseCode(code, width = width)
  }
  do.call(formatter, c(list(code), list(...)))
}

#' @export
#' @rdname formatCode
styleText <- function(code, ...) {
  # TODO: break up functionality in rebreak and allow user to opt-out?
  # Also, perhaps someday we let styler handle the %>% line-breaking?
  # https://github.com/r-lib/styler/issues/523
  code <- rebreak(code)
  styler::style_text(code, ...)
}


#' @inheritParams formatCode
#' @export
#' @rdname formatCode
deparseCode <- function(code, width = 500L) {
  code <- comment_flags_to_enclosings(code)
  code_text <- deparse_flatten(code, width = width)
  code_text <- comment_remove_enclosing(code_text)
  oldClass(code_text) <- "shinyMetaDeparsed"
  code_text
}

deparse_flatten <- function(expr, width = 500L) {
  if (rlang::is_call(expr, "{")) {
    paste0(vapply(expr[-1], deparse_flatten, character(1)), collapse = "\n")
  } else {
    # TODO: should this have `backtick = TRUE`?
    paste0(deparse(expr, width.cutoff = width), collapse = "\n")
  }
}

# Neither deparse() nor styler will go out of their way to break on %>%, and
# deparse will break on other random operators instead. This function inserts
# newlines after %>%, and replaces newlines that follow operators or commas with
# a single space. The resulting code string will not contain indentation, and
# must be processed further to be considered readable.
rebreak <- function(str) {
  str <- paste(str, collapse = "\n")
  tokens <- sourcetools::tokenize_string(str)
  tokens$value <- paste0(
    tokens$value,
    ifelse(
      tokens$type == "operator" & tokens$value == "%>%",
      "\n",
      ""
    )
  )
  # if the token appearing after a line-break is a
  # comma/operator, remove the line-break
  operator_newline <- grepl("\n", tokens$value) &
    tokens$type == "whitespace" &
    c(FALSE, head(tokens$type %in% c("comma", "operator"), -1))
  tokens$value[operator_newline] <- " "
  new_str <- paste(tokens$value, collapse = "")
  gsub("\\s*\\r?\\n\\s*", "\n", new_str)
}

# If a deparsed code string contains a line that's enclosed in our special
# identifiers, then turn it into a comment
comment_remove_enclosing <- function(x) {
  if (!is.character(x) || length(x) > 1) {
    stop("Expected a string (character vector of length 1).")
  }
  txt <- strsplit(x, "\n")[[1]]
  comment_index <- grep(paste0('^\\s*"', comment_start), txt)
  if (!length(comment_index)) return(txt)
  txt[comment_index] <- sub(paste0('^(\\s*)"', comment_start), "\\1", txt[comment_index])
  txt[comment_index] <- sub(paste0(comment_end, '"$'), "", txt[comment_index])
  # e.g. `deparse("a \"string\"")` -> "\"a \\\"string\\\"\""
  txt[comment_index] <- gsub("\\\"", "\"", txt[comment_index], fixed = TRUE)
  paste(txt, collapse = "\n")
}
