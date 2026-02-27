test_that("knit_expand_safe ignores calling environment", {

  foo <- "bar"
  expect_error(
    knit_expand_safe(text = "{{ foo }}"),
    "foo"
  )
  expect_identical(
    knit_expand_safe(text = "{{ foo }}", vars = list(foo = foo)),
    "bar"
  )

  # matches_before is (at the time of this writing) a local variable in the
  # knit_expand_safe, which is the parent.frame of knit_expand call. By default,
  # knit_expand would be able to "see" that variable; knit_expand_safe is
  # supposed to prevent that.
  expect_error(
    knit_expand_safe(text = "{{ matches_before }}"),
    "matches_before"
  )

  expect_identical(
    local({ x <- "whatever"; knit_expand_safe(text = "{{ x }}", vars = list(x = x)) }),
    "whatever"
  )

  expect_identical(
    knit_expand_safe(text = "{{ toupper('hello') }}"),
    "HELLO"
  )

  # Use one of knit_expand_safe's parameter names as a var
  expect_identical(
    knit_expand_safe(text = "{{ text }}", vars = list(text = "something")),
    "something"
  )
})

# Helper: parse code and extract the srcref of the { block inside the first
# metaReactive2() call (i.e., what exprToVarname passes to mrexprSrcrefToLabel).
# The srcref[[1]] of the { block points to the { on the metaReactive2 line.
get_mr_block_srcref <- function(code, src) {
  exprs <- parse(text = code, keep.source = TRUE, srcfile = src)
  # top expression: dataset <- metaReactive2({ ... })
  top <- exprs[[1]]
  # top[[3]] = metaReactive2({ ... })
  # top[[3]][[2]] = the { block }
  block <- top[[3]][[2]]
  block_sr <- attr(block, "srcref")
  # srcref[[1]] is the srcref of the { itself
  block_sr[[1]]
}

describe("mrexprSrcrefToLabel", {

  it("infers varname from a normal srcfilecopy (no #line directive)", {
    code <- c(
      "dataset <- metaReactive2({",
      "  metaExpr({ mtcars })",
      "})"
    )
    src <- srcfilecopy("app.R", code, isFile = TRUE)
    srcref <- get_mr_block_srcref(code, src)

    result <- shinymeta:::mrexprSrcrefToLabel(srcref, defaultLabel = "fallback")
    expect_equal(result, "dataset")
  })

  it("infers varname when #line directive shifts line numbers", {
    code <- c(
      "dataset <- metaReactive2({",
      "  metaExpr({ mtcars })",
      "})"
    )
    lines <- c('#line 1 "app.R"', code)
    src <- srcfilecopy("app.R", lines, isFile = TRUE)
    srcref <- get_mr_block_srcref(lines, src)

    result <- shinymeta:::mrexprSrcrefToLabel(srcref, defaultLabel = "fallback")
    expect_equal(result, "dataset")
  })

  it("infers varname when srcfile is a srcfilealias", {
    code <- c(
      "dataset <- metaReactive2({",
      "  metaExpr({ mtcars })",
      "})"
    )
    lines <- c('#line 1 "/absolute/path/to/app.R"', code)
    src <- srcfilecopy("app.R", lines, isFile = TRUE)
    srcref <- get_mr_block_srcref(lines, src)
    srcfile <- attr(srcref, "srcfile", exact = TRUE)

    # Verify we actually got a srcfilealias
    expect_s3_class(srcfile, "srcfilealias")
    expect_null(srcfile$lines)

    result <- shinymeta:::mrexprSrcrefToLabel(srcref, defaultLabel = "fallback")
    expect_equal(result, "dataset")
  })

  it("returns defaultLabel when metaReactive is not found", {
    code <- c(
      "x <- function() {",
      "  1 + 2",
      "}"
    )
    src <- srcfilecopy("app.R", code, isFile = TRUE)
    exprs <- parse(text = code, keep.source = TRUE, srcfile = src)
    # Get the { block from the function body
    func_body <- exprs[[1]][[3]][[4]]  # function body { block }
    block_sr <- attr(func_body, "srcref")
    srcref <- block_sr[[1]]

    result <- shinymeta:::mrexprSrcrefToLabel(srcref, defaultLabel = "fallback")
    expect_equal(result, "fallback")
  })

  it("handles metaReactive preceded by blank lines with #line directive", {
    code <- c(
      "",
      "",
      "dataset <- metaReactive2({",
      "  metaExpr({ mtcars })",
      "})"
    )
    lines <- c('#line 1 "app.R"', code)
    src <- srcfilecopy("app.R", lines, isFile = TRUE)
    srcref <- get_mr_block_srcref(lines, src)

    result <- shinymeta:::mrexprSrcrefToLabel(srcref, defaultLabel = "fallback")
    expect_equal(result, "dataset")
  })
})
