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

# return TRUE if return appears anywhere in a call tree, otherwise FALSE
has_return <- function(x) {
  if (!is.call(x)) return(FALSE)
  if (rlang::is_call(x, "return")) return(TRUE)
  any(vapply(x, has_return, logical(1)))
}


modify_call <- function(x, localize = "auto", unpack = TRUE) {

  if (rlang::is_call(x, "<-")) {
    # Modify a call like `x <- {1 + 1}` to `x <- 1 + 1`
    while (rlang::is_call(x[[3]], "{", n = 1)) {
      x[[3]] <- x[[3]][[2]]
    }

    if (rlang::is_call(x[[3]], "{")) {

      if (identical(localize, "auto")) {
        localize <- has_return(x[[3]])
      }

      if (localize) {

        x[[3]] <- call("local", x[[3]])

      } else if (unpack) {
        # Modify a call like:
        # x <- {
        #   a <- 1
        #   b <- 1 + a
        #   b + 1
        # }
        #  to
        # {
        #   a <- 1
        #   b <- 1 + a
        #   x <- b + 1
        # }
        rhs <- x[[3]]
        rhs[[length(rhs)]] <- call("<-", x[[2]], rhs[[length(rhs)]])
        x <- rhs
      }

    }

  }

  if (rlang::is_call(x, "{")) {

    # comment must appear as a direct child of a `{` call
    x[-1] <- lapply(x[-1], function(y) {
      if (is_comment(y)) prefix_class(y, "isComment") else y
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

  y <- switch(expr_type(x),
    constant = if (inherits(x, "isComment")) paste0(comment_start, x, comment_end) else x,
    # Recursive cases
    call = as.call(lapply(x, modify_call, localize = localize, unpack = unpack)),
    pairlist = as.pairlist(lapply(x, modify_call, localize = localize, unpack = unpack))
  )

  # if this is a case we don't recognize, return the input value
  y %||% x
}

is_comment <- function(x) {
  if (!is.character(x) || length(x) != 1) return(FALSE)
  grepl("^#", x)
}

comment_start <- "######StartOfShinyMetaCommentIdentifier######"
comment_end <- "######EndOfShinyMetaCommentIdentifier######"
