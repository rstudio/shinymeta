context("observe")

describe("metaObserve", isolate({
  it("basically works", {
    x <- 0
    mo <- metaObserve({
      x <<- 1
    })
    shiny:::flushReact()

    expect_identical(x, 1)
    expect_equal(withMetaMode(mo()), quote( x <<- 1 ))
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
    expect_equal(withMetaMode(mo()), quote( x <<- 1 ))
  })

  it("varies by dynvars", {

    mr <- metaReactive({
      cars
    })

    mo <- metaObserve({
      print(!!mr())
    })

    x <- expandCode(!!mo())
    expect_equal(
      x,
      quote( print(cars) )
    )

    x <- expandCode(!!mo(), patchCalls = list(mr = quote(boats)))
    expect_equal(
      x,
      quote( print(boats) )
    )

  })
}))
