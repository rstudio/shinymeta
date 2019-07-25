# --------------------------------------------------------
# Code formatting utilities
# --------------------------------------------------------

# wrap a call in local
add_local_scope <- function(x, localize) {
  if (!is.call(x)) return(x)
  if (identical(localize, "auto")) {
    localize <- any(unlist(has_return(x), use.names = FALSE))
  }
  if (localize) call("local", x) else x
}

# Returns TRUE if a return() is detected outside of
# an anonymous function or local() expresion
has_return <- function(x) {
  if (!is.call(x)) return(FALSE)
  if (rlang::is_call(x, "function")) return(FALSE)
  if (rlang::is_call(x, "local")) return(FALSE)
  if (rlang::is_call(x, "return")) return(TRUE)
  lapply(x, has_return)
}


# Modify a call like (also works with a collection of assignments)
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
bind_to_return <- function(x) {

  if (is_assign(x) && rlang::is_call(x[[3]], "{") && inherits(x[[3]], "bindToReturn")) {
    rhs <- x[[3]]
    rhs[[length(rhs)]] <- call("<-", x[[2]], rhs[[length(rhs)]])
    x <- rhs
  }

  walk_ast(x, bind_to_return)
}

# Modify a call like (also works with a collection of them)
# a <- {
#  "# my comment"
#  1+1
# }
# to
# {
#  "# my comment"
#  a <- 1+1
# }
elevate_comments <- function(x) {

  if (is_assign(x) && rlang::is_call(x[[3]], "{", n = 2)) {
    if (isTRUE(attr(x[[3]][[2]], "shinymeta_comment"))) {
      x <- call(
        "{", x[[3]][[2]], call("<-", x[[2]], x[[3]][[3]])
      )
    }
  }

  walk_ast(x, elevate_comments)
}


# Find and flag (i.e. attach attributes) to comment-like strings
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

  walk_ast(x, comment_flags)
}


# Find flagged comment strings and enclose that string with
# an identifier we remove during deparseCode().
comment_flags_to_enclosings <- function(x) {
  walk_ast(
    x,
    comment_flags_to_enclosings,
    constant = {
      if (isTRUE(attr(x, "shinymeta_comment"))) {
        paste0(comment_start, x, comment_end)
      } else if (length(attr(x, "shinymeta_comment"))) {
        structure(x, shinymeta_comment = NULL)
      } else {
        x
      }
    }
  )
}


# ---------------------------------------------------------
# Helpers
# ---------------------------------------------------------


# Inspired by Hadley Wickham's AST walking helpers
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

# Apply a function to each node of an AST
walk_ast <- function(x, fun, ..., constant = x, symbol = x) {
  switch(
    expr_type(x),
    constant = constant,
    symbol = symbol,
    call = {
      res <- as.call(lapply(x, fun, ...))
      if (inherits(x, "bindToReturn")) {
        prefix_class(res, "bindToReturn")
      } else {
        res
      }
    },
    pairlist = as.pairlist(lapply(x, fun, ...)),
    x
  )
}

is_assign <- function(x) {
  inherits(x, c("<-", "="))
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
