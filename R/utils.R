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
    if (is.call(func)) as.call(func) else as.symbol(func),
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

# Given the srcref to a metaReactive expression, attempts to figure out what the
# name of the reactive expression is. This isn't foolproof, as it literally
# scans the line of code that started the reactive block and looks for something
# that looks like assignment. If we fail, fall back to a default value (likely
# the block of code in the body of the reactive).
mrexprSrcrefToLabel <- function(srcref, defaultLabel) {
  if (is.null(srcref))
    return(defaultLabel)

  srcfile <- attr(srcref, "srcfile", exact = TRUE)
  if (is.null(srcfile))
    return(defaultLabel)

  if (is.null(srcfile$lines))
    return(defaultLabel)

  lines <- srcfile$lines
  # When pasting at the Console, srcfile$lines is not split
  if (length(lines) == 1) {
    lines <- strsplit(lines, "\n")[[1]]
  }

  if (length(lines) < srcref[1]) {
    return(defaultLabel)
  }

  firstLineIdx <- srcref[1]
  firstLine <- substring(lines[firstLineIdx], 1, srcref[2] - 1)
  while (!grepl("metaReactive", firstLine) & firstLineIdx >= 1) {
    firstLineIdx <- firstLineIdx - 1
    firstLine <- lines[firstLineIdx]
  }

  m <- regexec("(.*)(<-|=)\\s*metaReactive2?\\s*\\($", firstLine)
  if (m[[1]][1] == -1) {
    return(defaultLabel)
  }
  sym <- regmatches(firstLine, m)[[1]][2]
  res <- try(parse(text = sym), silent = TRUE)
  if (inherits(res, "try-error"))
    return(defaultLabel)

  if (length(res) != 1)
    return(defaultLabel)

  return(as.character(res))
}
