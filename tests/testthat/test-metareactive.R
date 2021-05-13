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
  })

  # Outputs
  testthat::skip_if_not_installed("shiny", "1.6.0.9000")
  isolate({
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

  it("errors on non-meta usage", {
    ma <- metaAction({})
    expect_error(ma())
  })

  it("can contain code that uses !!", {
    ma <- metaAction({
      foo <- 1
      x <- rlang::expr(!!foo)
    })
    expect_identical(x, 1)
    if (getRversion() < "3.5") {
      skip("Quoted !! isn't printed properly in R3.4 and lower.")
    } else {
      expect_snapshot_output(withMetaMode(ma()))
    }
  })

  it("obeys scoping rules", {
    # introduces scopes
    outer <- environment()
    i <- 0

    mr <- metaReactive({
      inner <- environment()
      expect_false(identical(inner, outer))

      i <<- i + 1
    })
    isolate(mr())

    expect_identical(i, 1)

    mr2 <- metaReactive2({
      inner <- environment()
      expect_false(identical(inner, outer))
      i <<- i + 1
      metaExpr({
        innermost <- environment()
        expect_true(identical(innermost, inner))
        i <<- i + 1
      })
    })
    isolate(mr2())

    expect_identical(i, 3)

    # In meta mode, the `metaExpr()` part of the reactive is quoted and
    # returned, not executed, so `i` only increments by 1.
    withMetaMode(mr2())
    expect_identical(i, 4)
  })
})
