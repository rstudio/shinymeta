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

  # When sourceUTF8() wraps code with a #line directive whose path differs from
  # the srcfilecopy filename, R creates a srcfilealias whose $lines is NULL.
  # The actual lines live in $original$lines.
  lines <- srcfile$lines
  if (is.null(lines) && inherits(srcfile, "srcfilealias")) {
    lines <- srcfile$original$lines
  }

  if (is.null(lines))
    return(defaultLabel)

  # When pasting at the Console, srcfile$lines is not split
  if (length(lines) == 1) {
    lines <- strsplit(lines, "\n")[[1]]
  }

  # When a #line directive is present, R extends srcrefs to 8 elements.
  # srcref[7] is the pre-remap line number — the actual position in the
  # srcfilecopy's lines array.
  lineIdx <- srcref[1]
  if (length(srcref) >= 7 && srcref[7] != srcref[1]) {
    lineIdx <- srcref[7]
  }

  if (length(lines) < lineIdx) {
    return(defaultLabel)
  }

  firstLineIdx <- lineIdx
  firstLine <- substring(lines[firstLineIdx], 1, srcref[2] - 1)
  while (length(firstLine) > 0 && !grepl("metaReactive", firstLine) && firstLineIdx > 1) {
    firstLineIdx <- firstLineIdx - 1
    firstLine <- lines[firstLineIdx]
  }

  if (length(firstLine) == 0 || !grepl("metaReactive", firstLine)) {
    return(defaultLabel)
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

# For R 3.3/3.4
is_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

# Version of knit_expand that doesn't search the parent frame, and detects when
# expansion results in unsafe Rmd input (i.e. the evaluation of {{expr}} should
# never introduce a chunk boundary or even a new inline chunk)
knit_expand_safe <- function(file, vars = list(), text = xfun::read_utf8(file), delim = c("{{", "}}")) {
  # The approach we take here is to detect all knitr md patterns before and
  # after expansion, and fail if anything was either added or removed. We tried
  # just testing the output of each {{expansion}} for the patterns, but, that
  # doesn't catch cases where an inline.code is started in one expansion and
  # finished in another (see test in test-report.R).

  # Code chunk delimiter regexes
  # TODO: Can we assume `md`?
  patterns <- unname(unlist(knitr::all_patterns$md))

  matches_before <- count_matches_by_pattern(text, patterns)

  # Create an environment that contains nothing but the variables we want to
  # make available for template expansion, plus .GlobalEnv.
  eval_envir <- list2env(vars, parent = .GlobalEnv)

  # Use a knitr hook to ensure that only the ... arguments plus stuff in the
  # global environment are available when evaluating {{/}} expressions.
  orig_eval_inline <- knitr::knit_hooks$get("evaluate.inline")
  knitr::knit_hooks$set(evaluate.inline = function(code, envir) {
    # ignore envir, as it includes the parent frame of `knit_expand` which we
    # explicitly do not want to be used for evaluation--only ... arguments to
    # knit_expand_safe should be used.
    orig_eval_inline(code, eval_envir)
  })
  on.exit(knitr::knit_hooks$set(evaluate.inline = orig_eval_inline), add = TRUE)

  res <- knitr::knit_expand(text = text, delim = delim)

  matches_after <- count_matches_by_pattern(xfun::split_lines(res), patterns)

  if (!identical(matches_before, matches_after)) {
    # The process of knit_expand-ing introduced new (or removed existing?) code
    # chunks
    stop("Can't build report--user input values must not contain code chunk delimiters")
  }

  res
}

# Returns a vector of length `length(pattern)`, where each element is the total
# number of times the corresponding pattern element was found in the character
# vector `string`.
#
# > count_matches_by_pattern(c("abc12", "def34", "5"), c("[a-z]", "[0-9]"))
# [1] c(6, 5)
count_matches_by_pattern <- function(string, pattern) {
  vapply(pattern, function(regex) {
    matches <- stringr::str_locate_all(string, regex)
    sum(vapply(matches, nrow, integer(1)))
  }, integer(1), USE.NAMES = FALSE)
}
