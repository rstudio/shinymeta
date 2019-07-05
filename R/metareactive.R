.globals <- new.env(parent = emptyenv())
.globals$dynamicVars <- list()
.globals$nextId = 0L

# This is a global hook for intercepting meta-mode reads of metaReactive/2.
# The first argument is the (delayed eval) code result, and rexpr is the
# metaReactive/2 object itself. If evaluation of x is not triggered by the
# hook function, then the metaReactive/2 code will not execute/be expanded.
#
# The return value should be a code object.
.globals$rexprMetaReadFilter <- function(x, rexpr) {
  x
}

#' Create a meta-reactive expression
#'
#' Create a [reactive()] that, when invoked with meta-mode activated
#' (i.e. called within [expandCode()] or [withMetaMode()]), returns a
#' code expression (instead of evaluating that expression and returning the value).
#'
#' @details If you wish to capture specific code inside of `expr` (e.g. ignore code
#' that has no meaning outside shiny, like [req()]), use `metaReactive2()` in combination
#' with `metaExpr()`. When using `metaReactive2()`, `expr` must return a `metaExpr()`.
#'
#' TODO: Document reasons why varname detection might fail.
#'
#' @param inline If `TRUE`, during code expansion, do not declare a variable for
#' this object; instead, inline the code into every call site. Use this to avoid
#' introducing variables for very simple expressions.
#'
#' @param varname An R variable name that this object prefers to be named when
#' its code is extracted into an R script.
#'
#' @inheritParams shiny::reactive
#' @inheritParams metaExpr
#' @export
#' @seealso [metaExpr()]
#' @examples
#'
#' options(shiny.suppressMissingContextError = TRUE)
#'
#' input <- list(x = 1)
#'
#' y <- metaReactive({
#'   req(input$x)
#'   a <- !!input$x + 1
#'   b <- a + 1
#'   c + 1
#' })
#'
#' withMetaMode(y())
#' expandCode(y <- !!y())
#'
#' y <- metaReactive2({
#'   req(input$x)
#'
#'   metaExpr({
#'     a <- !!input$x + 1
#'     b <- a + 1
#'     c + 1
#'   })
#' }, bindToReturn = TRUE)
#'
#' expandCode(y <- !!y())
#'
metaReactive <- function(expr, env = parent.frame(), quoted = FALSE,
  varname = NULL, domain = shiny::getDefaultReactiveDomain(), inline = FALSE,
  localize = "auto", bindToReturn = FALSE) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  varname <- exprToVarname(expr, varname, inline, "metaReactive")

  # Need to wrap expr with shinymeta:::metaExpr, but can't use rlang/!! to do
  # so, because we want to keep any `!!` contained in expr intact (i.e. too
  # early to perform expansion of expr here).
  #
  # Even though expr itself is quoted, wrapExpr will effectively unquote it by
  # interpolating it into the `metaExpr()` call, thus quoted = FALSE.
  expr <- wrapExpr(shinymeta::metaExpr, expr, env, quoted = FALSE, localize = localize, bindToReturn = bindToReturn)

  metaReactiveImpl(expr = expr, env = env, varname = varname, domain = domain, inline = inline)
}


#' @export
#' @rdname metaReactive
metaReactive2 <- function(expr, env = parent.frame(), quoted = FALSE,
  varname = NULL, domain = shiny::getDefaultReactiveDomain(), inline = FALSE) {

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  varname <- exprToVarname(expr, varname, inline, "metaReactive2")

  metaReactiveImpl(expr = expr, env = env, varname = varname, domain = domain, inline = inline)
}

exprToVarname <- function(expr, varname = NULL, inline, objectType = "metaReactive") {
  if (is.null(varname)) {
    if (inline) {
      return("anonymous")
    }
    srcref <- attr(expr, "srcref", exact = TRUE)
    if (is.null(srcref)) {
      stop("No srcref available; is your ", objectType, " code missing {curly braces}?", call. = FALSE)
    }
    varname <- mrexprSrcrefToLabel(srcref[[1]],
      stop("Failed to infer variable name for ", objectType, "; see the Details section of ?metaReactive for suggestions", call. = FALSE)
    )
  } else {
    if (!is.character(varname) || length(varname) != 1 || is.na(varname) || nchar(varname) == 0) {
      stop("varname must be a non-empty string", call. = FALSE)
    }
    if (varname != make.names(varname)) {
      stop("varname must be a valid R identifier (was '", varname, "')", call. = FALSE)
    }
  }
  varname
}

metaReactiveImpl <- function(expr, env, varname, domain, inline) {
  force(expr)
  force(env)
  force(varname)
  force(domain)
  force(inline)

  r_normal <- shiny::reactive(expr, env = env, quoted = TRUE, label = varname, domain = domain)
  r_meta <- function() {
    shiny::withReactiveDomain(domain, {
      rlang::eval_tidy(expr, NULL, env)
    })
  }

  self <- structure(
    function() {
      metaDispatch(
        normal = {
          r_normal()
        },
        meta = {
          .globals$rexprMetaReadFilter(r_meta(), self)
        }
      )
    },
    class = c("shinymeta_reactive", "function"),
    shinymetaVarname = varname,
    shinymetaUID = shiny:::createUniqueId(8),
    shinymetaDomain = domain,
    shinymetaInline = inline
  )
  self
}

#' @export
print.shinymeta_reactive <- function(x, ...) {
  cat("metaReactive:", attr(x, "shinymetaVarname"), "\n", sep = "")
}

# A global variable that can be one of three values:
# 1. FALSE - metaExpr() should return its EVALUATED expr
# 2. TRUE - metaExpr() should return its QUOTED expr
# 3. "mixed" - same as TRUE, but see below
#
# The "mixed" exists to serve cases like metaReactive2. In cases
# where calls to metaReactives are encountered inside of metaReactive2
# but outside of metaExpr, those metaReactives should be evaluated in
# non-meta mode (i.e. metaMode(FALSE)).
#
# See metaDispatch for more details on mixed mode.
metaMode <- local({
  value <- FALSE
  function(x) {
    if (missing(x)) {
      value
    } else {
      if (!isTRUE(x) && !isFALSE(x) && !identical(x, "mixed")) {
        stop("Invalid metaMode() value: legal values are TRUE, FALSE, and \"mixed\"")
      }
      value <<- x
    }
  }
})

# More-specific replacement for switch() on the value of metaMode().
#
# This gives us a single place to update if we need to modify the set of
# supported metaMode values.
switchMetaMode <- function(normal, meta, mixed) {
  if (missing(normal) || missing(meta) || missing(mixed)) {
    stop("switchMetaMode call was missing required argument(s)")
  }

  mode <- metaMode()
  if (isTRUE(mode)) {
    meta
  } else if (isFALSE(mode)) {
    normal
  } else if (identical(mode, "mixed")) {
    mixed
  } else {
    stop("Illegal metaMode detected: ", format(mode))
  }
}

# metaDispatch implements the innermost if/switch for meta-reactive objects:
# metaReactive/metaReactive2, metaObserve/metaObserve2, metaRender/metaRender2.
#
# We basically want to detect nested calls to `metaDispatch` without an
# intervening `withMetaMode(TRUE)` or `metaExpr`, and treat those cases as
# metaMode(FALSE).
#
# mr1 <- metaReactive({
#   1 + 1
# })
#
# mr2 <- metaReactive2({
#   mr1() # returns 2
#   !!mr1() # `!!`` is treated as double-boolean (NOT unquote), so: TRUE
#   metaExpr(
#     !!mr1() # returns quote(1 + 1)
#   )
# })
#
# withMetaMode(mr2())
metaDispatch <- function(normal, meta) {
  switchMetaMode(
    normal = {
      force(normal)
    },
    meta = {
      withMetaMode(meta, "mixed")
    },
    mixed = {
      withMetaMode(normal, FALSE)
    }
  )
}

metaCacheKey <- function() {
  list(.globals$dynamicVars, metaMode())
}


#' Evaluate an expression with meta mode activated
#'
#' @param expr an expression.
#' @param mode whether or not to evaluate expression in meta mode.
#'
#' @seealso [expandCode()]
#' @export
withMetaMode <- function(expr, mode = TRUE) {
  origVal <- metaMode()
  if (!identical(origVal, mode)) {
    metaMode(mode)
    on.exit(metaMode(origVal), add = TRUE)
  }

  if (!getOption("shiny.allowoutputreads", FALSE)) {
    op <- options(shiny.allowoutputreads = TRUE)
    on.exit(options(op), add = TRUE)
  }

  if (switchMetaMode(normal = FALSE, meta = TRUE, mixed = FALSE)) {
    expr <- prefix_class(expr, "shinyMetaExpr")
  }

  force(expr)
}

#' Mark an expression as a meta-expression
#'
#'
#'
#' @param expr An expression (quoted or unquoted).
#' @param env An environment.
#' @param quote Is the expression quoted? This is useful when you want to use an expression
#' that is stored in a variable; to do so, it must be quoted with [`quote()`].
#' @param localize Whether or not to wrap the returned expression in [`local()`].
#' The default, \code{"auto"}, only wraps expressions with a top-level [`return()`]
#' statement (i.e., return statements in anonymized functions are ignored).
#' @param bindToReturn For non-`localize`d expressions, should an assignment
#' of a meta expression be applied to the _last child_ of the top-level `\{` call?
#'
#' @seealso [metaReactive2()], [metaObserve2()], [metaRender2()]
#' @export
metaExpr <- function(expr, env = parent.frame(), quoted = FALSE, localize = "auto", bindToReturn = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  metaExpr_(expr, env = env, quoted = quoted, localize = localize, bindToReturn = bindToReturn)
}

metaExpr_ <- function(expr, env = parent.frame(), quoted = FALSE, localize = "auto", bindToReturn = FALSE,
  topLevelDynVars = TRUE) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  if (switchMetaMode(normal = TRUE, meta = FALSE, mixed = FALSE)) {
    expr <- expandExpr(expr, list(), env)
    return(rlang::eval_tidy(expr, env = env))
  }

  # metaExpr() moves us from mixed to meta state
  withMetaMode(mode = TRUE, {
    expr <- comment_flags(expr)
    expr <- expandExpr(expr, if (topLevelDynVars) .globals$dynamicVars, env)
    expr <- strip_outer_brace(expr)

    # Note that bindToReturn won't make sense for a localized call,
    # so determine we need local scope first, then add a special class
    # (we don't yet have the name for binding the return value)
    expr <- add_local_scope(expr, localize)

    # Apply bindToReturn rules, if relevant
    expr <- bind_to_return(expr)

    # TODO: let user opt-out of comment elevation
    # (I _think_ this is always safe)?
    expr <- elevate_comments(expr)

    # flag the call so that we know to bind next time we see this call
    # inside an assign call, we should modify it
    if (bindToReturn && rlang::is_call(expr, "{")) {
      expr <- prefix_class(expr, "bindToReturn")
    }

    expr <- prefix_class(expr, "shinyMetaExpr")

    expr
  })
}

withDynamicScope <- function(expr, ..., .list = list(...)) {
  if (length(.list) > 0) {
    if (is.null(names(.list)) || !all(nzchar(names(.list)))) {
      stop("withDynamicScope invoked with unnamed vars; all vars must be named")
    }

    oldVars <- .globals$dynamicVars
    .globals$dynamicVars <- c(oldVars[setdiff(names(oldVars), names(.list))], .list)
    on.exit(.globals$dynamicVars <- oldVars)
  }

  expr
  # TODO use promise domain
}

#' Expand meta primitives into user code
#'
#' This function provides the main entry point for generating user code
#' via meta-components (e.g., [metaReactive()], [metaObserve()], [metaRender()], etc).
#' It's similar to [withMetaMode()], but instead, quotes the `expr`, which allows you
#' to generate code from multiple meta-components via quasiquotation (e.g. [rlang::!!]).
#' When producing code from multiple meta-components, you may find that code produced from one
#' meta-component overlaps (i.e., repeats redundant computation) with another meta-component.
#' In that case, it's desirable to assign the return value of a meta-component to a variable, and
#' use that variable (i.e., symbol) in downstream code generated from other meta-components. This
#' can be done via the `patchCalls` argument which can replace the return value of
#' a meta-component with a relevant variable name.
#'
#' @inheritParams metaExpr
#' @param patchCalls a named list of quoted symbols. The names of the list
#' should match name(s) bound to relevant meta-component(s) found in `expr`
#' (e.g. `petal_width` in the example below). The quoted symbol(s) should
#' match variable name(s) representing the return value of the meta-component(s).
#'
#' @export
#' @seealso [withMetaMode()]
#' @examples
#'
#' options(shiny.suppressMissingContextError = TRUE)
#'
#' petal_width <- metaReactive({
#'   iris$Petal.Width
#' })
#'
#' mean_pw <- metaReactive({
#'   mean(!!petal_width())
#' })
#'
#' expandCode(
#'   {
#'     pw <- !!petal_width()
#'     !!mean_pw()
#'   },
#'   patchCalls = list(
#'     petal_width = quote(pw)
#'   )
#' )
#'
expandCode <- function(expr, env = parent.frame(), quoted = FALSE, patchCalls = list()) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  withMetaMode(
    withDynamicScope(
      metaExpr_(expr, env = env, quoted = quoted, localize = FALSE,
        bindToReturn = FALSE, topLevelDynVars = FALSE),
      .list = lapply(patchCalls, constf)
    )
  )
}

is_output_read <- function(expr) {
  is_dollar <- rlang::is_call(expr, name = "$", n = 2) &&
    rlang::is_symbol(expr[[2]], "output") &&
    rlang::is_symbol(expr[[3]])
  is_bracket <- rlang::is_call(expr, name = "[[", n = 2) &&
    rlang::is_symbol(expr[[2]], "output") &&
    is.character(expr[[3]])
  is_dollar || is_bracket
}

# Create an `lhs <- rhs` expression, unless lhs == "", in which case
# just return rhs.
#
# lhs should be either "", some other string (to be converted using as.name),
# or a language object (e.g. quote(foo) or quote(foo$bar)).
#
# rhs can be anything; either a simple value, or a language object.
#
# Return value will probably be a language object, but possibly not (e.g.
# make_assign_expr("", 10) would just return 10).
make_assign_expr <- function(lhs = "", rhs) {
  stopifnot(is.character(lhs) || is.language(lhs))

  if (is.character(lhs)) {
    if (lhs == "") {
      return(rhs)
    } else {
      lhs <- as.name(lhs)
    }
  }

  call("<-", lhs, rhs)
}

#' @param ... A collection of meta-reactives.
#' @param .env An environment.
#' @param .pkgs A character vector of packages to load before the expanded code.
#' @export
#' @rdname expandCode
expandObjects <- function(..., .env = parent.frame(), .pkgs) {
  exprs <- rlang::exprs(...)

  patchCalls <- list()

  objs <- mapply(names(exprs), exprs, FUN = function(nm, x) {

    if (is_comment(x)) {
      if (nzchar(nm)) {
        stop("expandObjects called with a named comment; only unnamed comments are supported")
      }
      attr(x, "shinymeta_comment") <- TRUE
      return(x)
    }

    # Do a sensible thing if someone has done `expandObjects(mr())` instead of `expandObjects(mr)`
    if (rlang::is_call(x) && length(x) == 1 && (is.symbol(x[[1]]) || is_output_read(x[[1]]))) {
      x <- x[[1]]
    }


    if (is.symbol(x)) {
      # Get the value pointed to by `x`. We'll need this to decide what rules we
      # apply to its expansion. Throws error if not found.
      val <- get(as.character(x), pos = .env, inherits = TRUE)

      # Observers and reactive expressions get different rules.
      is_observe <- inherits(val, "shinymeta_observer")
      is_reactive_expr <- inherits(val, "shinymeta_reactive")

      # Only metaObserve and metaReactive objects are supported
      if (!is_observe && !is_reactive_expr) {
        stop("expandObjects called with ", as.character(x), ", which has unrecognized object type ", deparse(class(val)))
      }

      # If metaReactive objects are passed without an explicit name, use the
      # name of the object itself as the name--this is the common case.
      if (is_reactive_expr && nm == "") {
        nm <- as.character(x)
      }

      # Reactive expressions always go into patchCalls; observers never do, even
      # if they're passed to us as named arguments, because there's no way they
      # can be validly referred to from other meta-reactive objects.
      if (is_reactive_expr) {
        patchCalls[[as.character(x)]] <<- as.name(nm)
      }

      rhs <- wrapExpr(`!!`, as.call(list(x)))
      return(make_assign_expr(nm, rhs))
    }

    if (is_output_read(x)) {
      output_obj <- withMetaMode(eval(x, envir = .env))
      if (is.null(output_obj)) {
        stop("Could not find ", format(x))
      }
      rhs <- wrapExpr(`!!`, as.call(list(x)))
      return(make_assign_expr(nm, rhs))
    }

    stop("expandObjects requires all arguments to be comment-strings and/or variable names of meta-reactive objects")
  })

  if (!missing(.pkgs)) {
    libs <- lapply(.pkgs, function(x) call("library", x))
    objs <- c(libs, objs)
  }

  expr <- do.call(call, c(list("{"), objs), quote = TRUE)
  expandCode(!!expandExpr(expr, NULL, .env), patchCalls = patchCalls)
}

#' @export
newExpansionContext <- function() {
  structure(
    list(
      uidToVarname = fastmap::fastmap(missing_default = NULL),
      seenVarname = fastmap::fastmap(missing_default = FALSE),
      # Function to make a (hopefully but not guaranteed to be new) varname
      makeVarname = local({
        nextVarId <- 0L
        function() {
          nextVarId <<- nextVarId + 1L
          paste0("var_", nextVarId)
        }
      })
    ),
    class = "shinymetaExpansionContext"
  )
}

#' @export
print.shinymetaExpansionContext <- function(x, ...) {
  map <- x$uidToVarname
  cat(sprintf("%s [id: %s]", map$mget(map$keys()), map$keys()), sep = "\n")
}

#' @export
expandChain <- function(..., .expansionContext = newExpansionContext()) {
  # As we come across previously unseen objects (i.e. the UID has not been
  # encountered before) we have to make some decisions about what variable name
  # (i.e. varname) to use to represent that object. This varname is either
  # auto-detected based on the metaReactive's variable name, or provided
  # explicitly by the user when the metaReactive is created. (If the object
  # belongs to a module, then we use the module ID to prefix the varname.)
  #
  # But, the desired variable name might already have been used by a different
  # metaReactive (i.e. two objects have the same label). In this case, we can
  # also use a var_1, var_2, etc. (and this is what the code currently does)
  # but it'd be even better to try to disambiguate by using the desired name
  # plus _1, _2, etc. (keep going til you find one that hasn't been used yet).
  #
  # IDEA:
  # A different strategy we could use is to generate a gensym as the label at
  # first, keeping track of the metadata for every gensym (label, module id).
  # Then after the code generation is done, we can go back and see what the
  # best overall set of variable names is. For example, if the same variable
  # name "df" is used within module IDs "one" and "two", we can use "one_df"
  # and "two_df"; but if only module ID "one" is used, we can just leave it
  # as "df". (As opposed to the current strategy, where if "one" and "two"
  # are both used, we end up with "df" and "df_two".)

  # Keep track of what label we have used for each UID we have previously
  # encountered. If a UID isn't found in this map, then we haven't yet
  # encountered it.
  uidToVarname <- .expansionContext$uidToVarname
  # Keep track of what labels we have used, so we can be sure we don't
  # reuse them.
  seenVarname <- .expansionContext$seenVarname

  # As we encounter metaReactives that we depend on (directly or indirectly),
  # we'll append their code to this list (including assigning them to a label).
  dependencyCode <- list()

  # Override the rexprMetaReadFilter while we generate code. This is a filter
  # function that metaReactive/metaReactive2 will call when someone asks them
  # for their meta value. The `x` is the (lazily evaluated) logic for actually
  # generating their code (or retrieving it from cache).
  oldFilter <- .globals$rexprMetaReadFilter
  .globals$rexprMetaReadFilter <- function(x, rexpr) {
    # Read this object's UID.
    uid <- attr(rexpr, "shinymetaUID", exact = TRUE)
    domain <- attr(rexpr, "shinymetaDomain", exact = TRUE)
    inline <- attr(rexpr, "shinymetaInline", exact = TRUE)

    if (isTRUE(inline)) {
      # The metaReactive doesn't want to have its own variable
      return(x)
    }

    # Check if we've seen this UID before, and if so, just return the same
    # varname as we used last time.
    varname <- uidToVarname$get(uid)
    if (!is.null(varname)) {
      return(as.symbol(varname))
    }

    # OK, we haven't seen this UID before. We need to figure out what variable
    # name to use.

    # Our first choice would be whatever varname the object itself has (the true
    # var name of this metaReactive, or a name the user explicitly provided).
    varname <- attr(rexpr, "shinymetaVarname", exact = TRUE)

    # If there wasn't either a varname or explicitly provided name, just make
    # a totally generic one up.
    if (is.null(varname) || varname == "" || length(varname) != 1) {
      varname <- .expansionContext$makeVarname()
    } else {
      if (!is.null(domain)) {
        varname <- gsub("-", "_", domain$ns(varname))
      }
    }

    # Make sure we don't use a variable name that has already been used.
    while (seenVarname$get(varname)) {
      varname <- .expansionContext$makeVarname()
    }

    # Remember this UID/varname combination for the future.
    uidToVarname$set(uid, varname)
    # Make sure this varname doesn't get used again.
    seenVarname$set(varname, TRUE)

    # Since this is the first time we're seeing this object, now we need to
    # generate its code and store it in our running list of dependencies.
    expr <- rlang::expr(`<-`(!!as.symbol(varname), !!x))
    dependencyCode <<- c(dependencyCode, list(expr))

    # This is what we're returning to the caller; whomever wanted the code for
    # this metaReactive is going to get this variable name instead.
    as.symbol(varname)
  }
  on.exit(.globals$rexprMetaReadFilter <- oldFilter, add = TRUE, after = FALSE)

  withMetaMode({
    # Trigger evaluation of the ..., which will also cause dependencyCode to be
    # populated. The value of list(...) should all be code expressions, unless
    # the user passed in something wrong.
    # TODO: Validate that all values are code expressions
    # TODO: If arguments are named, turn those into assignment, probably?
    # TODO: If we turn named arguments into assignment, we need to make sure
    #   that downstream objects use that variable name instead of the one based
    #   on the object's varname.
    # TODO: Filter out NULL values in res.
    dot_args <- eval(substitute(alist(...)))
    res <- lapply(seq_along(dot_args), function(i) {
      this_code <- eval(as.symbol(paste0("..", i)), envir = environment())
      myDependencyCode <- dependencyCode
      dependencyCode <<- list()
      c(myDependencyCode, list(this_code))
    })
    res <- unlist(res, recursive = FALSE)
    res <- res[!vapply(res, is.null, logical(1))]

    # Expand into a block of code
    metaExpr({!!!res})
  })
}

prefix_class <- function (x, y) {
  # Can't set attributes on a symbol, but that's alright because
  # we don't need to flag or compute on symbols
  if (is.symbol(x) || !is.language(x)) return(x)
  oldClass(x) <- unique(c(y, oldClass(x)))
  x
}

remove_class <- function(x, y) {
  if (is.symbol(x) || !is.language(x)) return(x)
  oldClass(x) <- setdiff(oldClass(x), y)
  x
}

quotedList <- function(...) {
  enquote(...)
}
