context("render")

describe("metaRender", isolate({
  it("basically works", {
    expect_identical(
      shiny::renderText({ paste("foo", "bar") })(),
      "foo bar"
    )

    expect_identical(
      metaRender(shiny::renderText, { paste("foo", "bar") })(),
      "foo bar"
    )

    expect_equal(
      withMetaMode({
        metaRender(shiny::renderText, { paste("foo", "bar") })()
      }),
      quote(
        paste("foo", "bar")
      )
    )

    expect_equal(
      withMetaMode({
        metaRender2(shiny::renderText, metaExpr({ paste("foo", "bar") }))()
      }),
      quote(
        paste("foo", "bar")
      )
    )

  })

  it("works with quoted expr", {
    x <- local({
      a <- "foo"
      b <- "bar"
      env <- environment()
      list(
        expr0 = quote({ paste(a, b) }),
        expr1 = quote({ paste(!!a, !!b) }),
        expr2 = quote(metaExpr({ paste(!!a, !!b) })),
        env = env
      )
    })

    expect_identical(
      shiny::renderText(expr = x$expr0, env = x$env, quoted = TRUE)(),
      "foo bar"
    )

    expect_equal(
      withMetaMode({
        metaRender(shiny::renderText, expr = x$expr1, env = x$env, quoted = TRUE)()
      }),
      quote(
        paste("foo", "bar")
      )
    )

    expect_equal(
      withMetaMode({
        metaRender2(shiny::renderText, expr = x$expr2, env = x$env, quoted = TRUE)()
      }),
      quote(
        paste("foo", "bar")
      )
    )

  })


  it("removes curly brackets when appropriate", {
    mr1 <- metaReactive({1 + 1})
    code <- expandChain(invisible(mr1()))
    expect_true(formatCode(code) == "mr1 <- 1 + 1")

    mr2 <- metaReactive({
      !!quote({1 + 1})
    })

    code <- expandChain(invisible(mr2()))

    expect_true(formatCode(code) == "mr2 <- 1 + 1")
  })

}))
