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
      unclass(withMetaMode({
        metaRender(shiny::renderText, { paste("foo", "bar") })()
      })),
      quote(
        paste("foo", "bar")
      )
    )

    expect_equal(
      unclass(withMetaMode({
        metaRender2(shiny::renderText, metaExpr({ paste("foo", "bar") }))()
      })),
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
        expr1 = quote({ paste(..(a), ..(b)) }),
        expr2 = quote(metaExpr({ paste(..(a), ..(b)) })),
        env = env
      )
    })

    expect_identical(
      shiny::renderText(expr = x$expr0, env = x$env, quoted = TRUE)(),
      "foo bar"
    )

    expect_equal(
      unclass(withMetaMode({
        metaRender(shiny::renderText, expr = x$expr1, env = x$env, quoted = TRUE)()
      })),
      quote(
        paste("foo", "bar")
      )
    )

    expect_equal(
      unclass(withMetaMode({
        metaRender2(shiny::renderText, expr = x$expr2, env = x$env, quoted = TRUE)()
      })),
      quote(
        paste("foo", "bar")
      )
    )

  })

  it("works with a render pipeline", {
    output <- list()

    data <- metaReactive({ dplyr::sample_n(diamonds, 1000) })
    output$plot <- metaRender(renderPlot, {
      ggplot(..(data()), aes(carat, price)) + geom_point()
    })
    x1 <- expandChain(
      "# top-level comment",
      output$plot()
    )
    x2 <- expandChain(
      "# top-level comment",
      output[["plot"]]()
    )

    expect_snapshot_output(cran = TRUE, formatCode(x1))
    expect_snapshot_output(cran = TRUE, formatCode(x2))

    # TODO: it would be nice to have an informative error here
    # https://github.com/rstudio/shinymeta/issues/49
    #expect_error(expandChain(output$foo()), regexp = "output\\$foo")
  })


  it("removes curly brackets when appropriate", {
    mr1 <- metaReactive({1 + 1})
    code <- expandChain(invisible(mr1()))
    expect_true(formatCode(code) == "mr1 <- 1 + 1")

    mr2 <- metaReactive({
      ..(quote({1 + 1}))
    })

    code <- expandChain(invisible(mr2()))

    expect_true(formatCode(code) == "mr2 <- 1 + 1")
  })

  it("uses correct scopes", {
    outer_var <- 100

    renderer <- metaRender(renderText, {
      expect_identical(outer_var, 100)
      outer_var <<- outer_var + 1
    })
    renderer()
    expect_identical(outer_var, 101)

    mr <- metaReactive({
      expect_identical(outer_var, 101)
      outer_var <<- outer_var + 1
    })
    mr()
    expect_identical(outer_var, 102)

    renderer <- metaRender2(renderText, {
      expect_identical(outer_var, 102)
      outer_var <<- outer_var + 1
      metaExpr(outer_var <<- outer_var + 1)
    })
    renderer()
    expect_identical(outer_var, 104)

    mr <- metaReactive2({
      expect_identical(outer_var, 104)
      outer_var <<- outer_var + 1
      metaExpr(outer_var <<- ..(outer_var + 1))
    })
    mr()
    expect_identical(outer_var, 106)

    outer_var <- 104
    res <- withMetaMode(mr())
    expect_identical(outer_var, 105)
    expect_equal(unclass(res), quote(outer_var <<- 106))
  })

}))
