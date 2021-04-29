test_that("doesn't break metaprogramming with quosures", {
  isolate({

    my_quo <- local({
      local_x <- 123L
      rlang::quo({
        "# A comment in a quosure"
        local_x
      })
    })

    outer_quo <- rlang::quo({
      if (!!my_quo == 123L) {
        "ok"
      }
    })

    # Reactive expressions

    r1 <- rlang::inject(metaReactive(!!my_quo, varname = "r1"))
    r2 <- rlang::inject(metaReactive(!!my_quo * -1L, varname = "r2"))

    expect_identical(r1(), 123L)
    expect_identical(r2(), -123L)

    expect_snapshot_output(formatCode(withMetaMode(r1())))
    expect_snapshot_output(formatCode(withMetaMode(r2())))

    # Observers

    result1 <- NULL
    o1 <- rlang::inject(metaObserve({
      result1 <<- !!outer_quo
    }))

    shiny:::flushReact()

    expect_identical(result1, "ok")

    expect_snapshot_output(formatCode(withMetaMode(o1())))

    # Outputs

    out1 <- rlang::inject(metaRender(shiny::renderText, !!outer_quo))
    expect_identical(out1(), "ok")

  })
})
