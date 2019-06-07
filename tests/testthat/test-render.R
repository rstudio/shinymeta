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
      quote({
        paste("foo", "bar")
      })
    )

    expect_equal(
      withMetaMode({
        metaRender2(shiny::renderText, metaExpr({ paste("foo", "bar") }))()
      }),
      quote({
        paste("foo", "bar")
      })
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
      quote({
        paste("foo", "bar")
      })
    )

    expect_equal(
      withMetaMode({
        metaRender2(shiny::renderText, expr = x$expr2, env = x$env, quoted = TRUE)()
      }),
      quote({
        paste("foo", "bar")
      })
    )

  })

  it("varies by dynvars", {
    mr <- metaReactive(cars)

    out <- metaRender(shiny::renderPrint, { str(!!mr()) })

    # Have to call expandCode outside of expect_equal because expect_equal will
    # expand the !!
    x <- expandCode({ !!out() })
    expect_equal(format_tidy_code(x), "str(cars)")

    x <- expandCode({ !!out() }, patchCalls = list(mr = quote(boats)))
    expect_equal(x, quote({{ str(cars) }}))
    expect_equal(format_tidy_code(x), "str(boats)")
  })
}))
