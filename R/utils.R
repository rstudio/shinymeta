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

# Expands (i.e. evaluated) all ..() function calls in an expression
# which mainly useful for unquoting away reactive inputs/values
# when generating code in meta-mode
expandExpr <- function(expr, env) {
  walk_ast(expr, preorder = TRUE, function(x) {
    if (!rlang::is_call(x, "..")) return(x)

    # make sure ..() contains a single unnamed argument
    if (!rlang::is_call(x, "..", n = 1)) {
      stop("..() must contain a single argument.")
    }
    if (!is.null(names(x))) {
      stop("..() cannot contain a named argument: '", names(x)[2], "'.")
    }
    # unquote
    x <- eval(x[[2]], list(), env)
    # Expand symbols to code that generates that symbol, as opposed
    # to just the symbol itself
    if (inherits(x, "shinymeta_symbol")) {
      as.symbol(x)
    } else if (is.symbol(x)) {
      call("as.symbol", as.character(x))
    } else {
      x
    }
  })
}

cleanExpr <- function(expr) {
  walk_ast(expr, function(x) {
    if (rlang::is_call(x, "..", n = 1) && is.null(names(x))) {
      x <- x[[2]]
    }
    x
  })
}


strip_outer_brace <- function(expr) {
  while (rlang::is_call(expr, "{", n = 1)) {
    expr <- expr[[2]]
  }
  expr
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

  m <- regexec("(.*)(<-|=)\\s*(?:shinymeta::)?metaReactive2?\\s*\\(", firstLine)
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
