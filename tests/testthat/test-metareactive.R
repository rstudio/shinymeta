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

    expect_snapshot_output(withMetaMode(r1()))
    expect_snapshot_output(withMetaMode(r2()))

    # Observers

    result1 <- NULL
    o1 <- rlang::inject(metaObserve({
      result1 <<- !!outer_quo
    }))

    shiny:::flushReact()

    expect_identical(result1, "ok")

    expect_snapshot_output(withMetaMode(o1()))

    # Outputs

    out1 <- rlang::inject(metaRender(shiny::renderText, !!outer_quo))
    expect_identical(out1(), "ok")
  })
})

describe("metaAction", {
  it("basically works", {
    a <- 1
    metaAction({
      a <- 2
    })
    expect_identical(a, 2)

    metaAction(quote({
      a <- 3
    }), quoted = TRUE)
    expect_identical(a, 3)

    env <- new.env()
    metaAction({
      a <- 4
    }, env = env)
    expect_identical(a, 3)
    expect_identical(env[["a"]], 4)

    metaAction(quote({
      a <- 5
    }), env = env, quoted = TRUE)
    expect_identical(a, 3)
    expect_identical(env[["a"]], 5)

    act <- metaAction(
      a <- 6,
      run = FALSE
    )
    expect_identical(a, 3)
    expect_snapshot_output(withMetaMode(act()))
  })

  it("unquotes properly", {
    b <- TRUE
    act <- metaAction(x <- ..(b))
    expect_identical(x, TRUE)
    expect_snapshot_output(withMetaMode(act()))

    mr <- metaReactive({
      FALSE
    })
    isolate({
      act <- metaAction(y <- ..(mr()))
    })
    expect_identical(y, FALSE)
    expect_snapshot_output(expandChain(act()))
  })
})
