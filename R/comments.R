# From Hadley Wickham's AST walking helpers
# https://adv-r.hadley.nz/expressions.html#ast-funs
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x), ...) %||% x
}

comment_identifier_add <- function(x) {

  if (length(x) > 1 && identical(x[[1]], quote(`{`))) {

    # comment must appear as a direct child of a `{` call
    x[-1] <- lapply(x[-1], function(y) {
      if (is_comment(y)) y <- prefix_class(y, "isComment")
      y
    })

    # if the comment appears as the last child of a `{` call,
    # it might be an assignment value, so we throw a warning if that occurs
    # and add a special class to the string so that when we arrive at the string
    # in the future, we know not to add the special comment identifier
    if (is_comment(x[[length(x)]])) {
      warning("A shinymeta comment can not appear as the last child of a `{` call")
      x[[length(x)]] <- remove_class(x[[length(x)]], "isComment")
    }

  }

  switch_expr(x,
    constant = if (inherits(x, "isComment")) paste0(comment_start, x, comment_end) else x,
    # Recursive cases
    call = as.call(lapply(x, comment_identifier_add)),
    pairlist = as.pairlist(lapply(x, comment_identifier_add))
  )
}

is_comment <- function(x) {
  if (!is.character(x) || length(x) > 1) return(FALSE)
  grepl("^#", x)
}

comment_start <- "######StartOfShinyMetaCommentIdentifier######"
comment_end <- "######EndOfShinyMetaCommentIdentifier######"
