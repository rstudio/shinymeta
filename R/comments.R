# Derived from Hadley Wickham's AST walking helpers
# https://adv-r.hadley.nz/expressions.html#ast-funs

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    if (is_comment(x)) "comment" else "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    if (length(x) > 1 && identical(x[[1]], quote(`{`)) && is_comment(x[[length(x)]])) {
      warning("A shinymeta comment can not appear as the last child of a `{` call")
    }
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

is_comment <- function(x) {
  if (!is.character(x) || length(x) > 1) return(FALSE)
  grepl("^\\s*#", x)
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

comment_identifier_add <- function(x) {
  switch_expr(x,
    # Base cases
    comment = paste0(comment_start, x, comment_end),
    constant = x,
    symbol = x,
    # Recursive cases
    call = as.call(lapply(x, comment_identifier_add)),
    pairlist = as.pairlist(lapply(x, comment_identifier_add))
  )
}


comment_start <- "######StartOfShinyMetaCommentIdentifier######"
comment_end <- "######EndOfShinyMetaCommentIdentifier######"
