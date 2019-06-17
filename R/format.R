#' Format code objects
#'
#' Converts language/expression objects (i.e. quoted code) into pretty-formatted
#' text. This logic is based on [styler::style_text()], but with additional
#' features/opinions; see Details.
#'
#' `formatCode` differs from [styler::style_text()] in a few ways:
#'
#' * Pipe operators (`%>%`) are always followed by a line break.
#' * Superfluous `\{` and `\}` are removed in many cases.
#' * Since quoted R code cannot contain comments, you can use comment strings.
#'
#' Comment strings are literal strings that appear on their own line, and begin
#' with one or more `#` characters. Such strings will be converted into comments
#' by `formatCode`.
#'
#' @param code Language/expr object (recommended), or character vector
#' @return Single-element character vector with formatted code
#' @export
formatCode <- function(code) {
  code_txt <- styler::style_text(rebreak(code))
  paste(code_txt, collapse = "\n")
}

deparse_flatten <- function(expr, width.cutoff = 500L) {
  if (rlang::is_call(expr, "{")) {
    paste0(vapply(expr[-1], deparse_flatten, character(1)), collapse = "\n")
  } else {
    # TODO: should this have `backtick = TRUE`?
    paste0(deparse(expr, width.cutoff = width.cutoff), collapse = "\n")
  }
}


# Neither deparse() nor styler will go out of their way to break on %>%, and
# deparse will break on other random operators instead. This function inserts
# newlines after %>%, and replaces newlines that follow operators or commas with
# a single space. The resulting code string will not contain indentation, and
# must be processed further to be considered readable.
rebreak <- function(str) {
  str <- modify_call(str)
  if (is.call(str) || is.symbol(str)) {
    str <- deparse_flatten(str)
  }
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
  new_str <- gsub("\\s*\\r?\\n\\s*", "\n", new_str)
  comment_identifier_remove(new_str)
}

# If a string appears entirely on it's own line,
# and begins with #, turn it into a comment
comment_identifier_remove <- function(x) {
  if (!is.character(x) || length(x) > 1) {
    stop("Expected a string (character vector of length 1).")
  }
  txt <- strsplit(x, "\n")[[1]]
  comment_index <- grep(paste0('^"', comment_start), txt)
  if (!length(comment_index)) return(txt)
  txt[comment_index] <- sub(paste0('^"', comment_start), "", txt[comment_index])
  txt[comment_index] <- sub(paste0(comment_end, '"$'), "", txt[comment_index])
  # e.g. `deparse("a \"string\"")` -> "\"a \\\"string\\\"\""
  txt[comment_index] <- gsub("\\\"", "\"", txt[comment_index], fixed = TRUE)
  paste(txt, collapse = "\n")
}
