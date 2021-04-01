describe("metaObserve", isolate({
  it("basically works", {
    x <- 0
    mo <- metaObserve({
      x <<- 1
    })
    shiny:::flushReact()

    expect_identical(x, 1)
    expect_equal(unclass(withMetaMode(mo())), quote( x <<- 1 ))
  })

  it("basically works 2", {
    x <- 0
    mo <- metaObserve2({
      x <<- x + 1
      metaExpr({
        x <<- ..(x + 1)
      })
    })
    shiny:::flushReact()

    expect_identical(x, 2)

    # The value becomes 3 here because even `withMetaMode(mo())` has a side effect
    # of x <<- x + 1 (the part outside the metaExpr)
    expect_equal(unclass(withMetaMode(mo())), quote( x <<- 3 ))
  })

}))
