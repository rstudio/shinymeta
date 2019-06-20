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

# Crawl the AST to find and flag (i.e. attach attributes) to
# strings that are eligible to become comments.
comment_flags <- function(x) {

  if (rlang::is_call(x, "{")) {

    # comment must appear as a direct child of a `{` call
    x[-1] <- lapply(x[-1], function(y) {
      if (is_comment(y) && !is_illegal(y)) attr(y, "shinymeta_comment") <- TRUE
      y
    })

    # If the comment appears as the last child of a `{` call,
    # it might be an assignment value, so we throw a warning if that occurs
    # and tag it so that if and when we arrive at the string in the future,
    # we know not to add the special comment identifier.
    if (is_comment(x[[length(x)]])) {
      warning("A shinymeta comment can not appear as the last child of a `{` call")
      attr(x[[length(x)]], "shinymeta_comment") <- "illegal"
    }

  }

  switch(expr_type(x),
    call = as.call(lapply(x, comment_flags)),
    pairlist = as.pairlist(lapply(x, comment_flags)),
    x
  )
}


# Walk the AST to find strings that are eligible to become comments
# and wrap the string with an enclosing that we grep for after
# the expression has been deparsed.
comment_flags_to_enclosings <- function(x) {
  switch(
    expr_type(x),
    constant = {
      if (isTRUE(attr(x, "shinymeta_comment"))) {
        paste0(comment_start, x, comment_end)
      } else if (length(attr(x, "shinymeta_comment"))) {
        structure(x, shinymeta_comment = NULL)
      } else {
        x
      }
    },
    # Recursive cases
    call = as.call(lapply(x, comment_flags_to_enclosings)),
    pairlist = as.pairlist(lapply(x, comment_flags_to_enclosings)),
    x
  )
}

is_comment <- function(x) {
  if (!is.character(x) || length(x) != 1) return(FALSE)
  grepl("^#", x)
}

is_illegal <- function(x) {
  identical(attr(x, "shinymeta_comment"), "illegal")
}

comment_start <- "######StartOfShinyMetaCommentIdentifier######"
comment_end <- "######EndOfShinyMetaCommentIdentifier######"
