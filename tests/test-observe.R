context("observe")

describe("metaObserve", {
  it("basically works", {
    x <- 0
    mo <- metaObserve({
      x <<- 1
    })
    shiny:::flushReact()

    expect_identical(x, 1)
    expect_identical(withMetaMode(mo()), quote({ x <<- 1 }))
  })

  it("basically works 2", {
    x <- 0
    mo <- metaObserve2({
      metaExpr({
        x <<- 1
      })
    })
    shiny:::flushReact()

    expect_identical(x, 1)
    expect_identical(withMetaMode(mo()), quote({ x <<- 1 }))
  })
})
