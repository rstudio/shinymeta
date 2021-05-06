describe("metaObserve", isolate({
  it("basically works", {
    e1 <- environment()
    x <- 0
    mo <- metaObserve({
      x <<- 1
    })
    mo1 <- metaObserve({
      e2 <- environment()
      expect_false(identical(e1, e2))
    })
    shiny:::flushReact()

    expect_identical(x, 1)
    expect_equal(unclass(withMetaMode(mo())), quote( x <<- 1 ))
  })

  it("basically works 2", {
    e1 <- environment()
    x <- 0
    mo <- metaObserve2({
    e2 <- environment()
    expect_false(identical(e1, e2))

      x <<- x + 1
      metaExpr({
        x <<- ..(x + 1)
      })
    })
    shiny:::flushReact()

    expect_identical(x, 2)

    # The value becomes 4 here because even `withMetaMode(mo())` has a side effect
    # of x <<- x + 1 (the part outside the metaExpr)
    res <- withMetaMode(mo())
    expect_equal(unclass(res), quote( x <<- 4 ))
  })

  it("obeys scoping rules", {
    # introduces scopes
    outer <- environment()
    i <- 0

    mo <- metaObserve({
      inner <- environment()
      expect_false(identical(inner, outer))

      i <<- i + 1
    })
    shiny:::flushReact()

    expect_identical(i, 1)

    mo2 <- metaObserve2({
      inner <- environment()
      expect_false(identical(inner, outer))
      i <<- i + 1
      metaExpr({
        innermost <- environment()
        expect_true(identical(innermost, inner))
        i <<- i + 1
      })
    })
    shiny:::flushReact()

    expect_identical(i, 3)

    withMetaMode(mo2())
    expect_identical(i, 4)
  })

}))
