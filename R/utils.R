#' @export
constf <- function(value) {
  force(value)
  function() {
    value
  }
}

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

wrapExpr <- function(func, ...) {
  func <- substitute(func)

  as.call(list(
    as.call(func),
    ...
  ))
}

# Takes an expr that contains !!, and expands the expr so that all !! are
# resolved. This is similar to rlang::quo except that 1) expr is already
# quoted, and 2) you can specify the data/env from which !! should be
# resolved.
expandExpr <- function(expr, data, env) {
  wrappedExpr <- wrapExpr(rlang::quo, expr)
  rlang::quo_get_expr(eval(wrappedExpr, data, env))
}

# local({
#   a <- quote(one)
#   b <- quote(three)
#   env <- environment()
#   expandExpr(quote(!!a + !!b), list(a = quote(two)), env)
# })

#' @export
format_tidy_code <- function(code_str) {
  code_txt <- styler::style_text(rebreak(code_str))
  paste(code_txt, collapse = "\n")
}

deparse_flatten <- function(expr, width.cutoff = 500L) {
  if (is.call(expr) && length(expr) > 1 && identical(expr[[1]], quote(`{`))) {
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
  str <- comment_identifier_add(str)
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
