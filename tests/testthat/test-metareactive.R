context("test-metareactive")

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
    expect_equal(withMetaMode(act()), quote(x <- TRUE))

    mr <- metaReactive({
      FALSE
    })
    isolate({
      act <- metaAction(y <- ..(mr()))
    })
    expect_identical(y, FALSE)
    expect_equal(expandChain(act()), quote({
      mr <- FALSE
      y <- mr
    }))
  })

  it("errors on non-meta usage", {
    ma <- metaAction({})
    expect_error(ma())
  })
})
