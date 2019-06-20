# Creates a nullary function that returns the given value
constf <- function(value) {
  force(value)
  function() {
    value
  }
}

#' Wrap expressions with a function call
#'
#' Creates a function call with the given function, using the quoted expressions
#' as unnamed arguments.
#'
#' @param func Fully-qualified function (i.e. base::head or shiny:::withLocalOptions)
#' @param ... Language objects to pass to the function
#' @noRd
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
# resolved. For example,
#
# local({
#   a <- quote(one)
#   b <- quote(three)
#   env <- environment()
#   expandExpr(quote(!!a + !!b), list(a = quote(two)), env)
# })
expandExpr <- function(expr, data, env) {
  wrappedExpr <- wrapExpr(rlang::quo, expr)
  rlang::quo_get_expr(eval(wrappedExpr, data, env))
}


strip_outer_brace <- function(expr) {
  while (rlang::is_call(expr, "{", n = 1)) {
    expr <- expr[[2]]
  }
  expr
}



elevate_comments <- function(expr) {
  # try elevate comments inside a `{` call
  if (rlang::is_call(expr, "{")) {
    return(as.call(c(list(quote(`{`)), lapply(expr[-1], elevate_comments))))
  }

  # transform a call like
  # a <- {
  #  "# my comment"
  #  1+1
  # }
  # to
  # {
  # "# my comment"
  # a <- 1+1
  # }

  # TODO: do this for `=` assignment as well
  if (rlang::is_call(expr, "<-") && rlang::is_call(expr[[3]], "{", n = 2)) {
    if (isTRUE(attr(expr[[3]][[2]], "shinymeta_comment"))) {
      expr <- call(
        "{", expr[[3]][[2]], call("<-", expr[[2]], expr[[3]][[3]])
      )
    }
  }

  expr
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
