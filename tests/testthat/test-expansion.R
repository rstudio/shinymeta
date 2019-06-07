context("expansion")


describe("expansion", isolate({
  one <- metaReactive({
    1
  })
  two <- metaReactive({
    !!one()
  })

  it("basically works", {
    res <- withMetaMode(
      metaExpr(!!two())
    )
    expect_equal(format_tidy_code(res), "1")
  })

  it("varies cache according to patchCalls", {
    res2 <- expandCode(!!two(),
      patchCalls = list(one = quote(x))
    )
    expect_equal(res2, quote({x}))
  })

  it("actually caches", {
    rand <- metaReactive({
      !!runif(1)
    })

    x1 <- withMetaMode(metaExpr(!!rand()))
    x2 <- withMetaMode(metaExpr(!!rand()))
    expect_identical(x1, x2)
  })
}))
