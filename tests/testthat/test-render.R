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
    q1 <- quote({{ str(cars) }})
    expect_equal(x, q1)
    expect_equal(formatCode(x), "str(cars)")

    x <- expandCode({ !!out() }, patchCalls = list(mr = quote(boats)))

    q2 <- quote({{ str(boats) }})
    expect_equal(x, q2)
    expect_equal(formatCode(x), "str(boats)")
  })


  it("removes curly brackets when appropriate", {
    mr1 <- metaReactive({
      1 + 1
    })

    code <- expandCode({
      mr1 <- !!mr1()
    })

    expect_equal(formatCode(code), "mr1 <- 1 + 1")

    mr2 <- metaReactive({
      !!quote({1 + 1})
    })

    code <- expandCode({
      mr2 <- !!mr2()
    })

    expect_equal(formatCode(code), "mr2 <- 1 + 1")

    code <- expandCode({
      mr1 <- !!mr1()
      mr2 <- !!mr2()
    })

    expect_equal(
      formatCode(code),
      "mr1 <- 1 + 1\nmr2 <- 1 + 1"
    )
  })

}))
