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
    q1 <- quote(1)
    expect_equal(res, q1)
    expect_true(formatCode(res) == "1")
  })

  it("varies cache according to patchCalls", {
    res2 <- expandCode(!!two(),
      patchCalls = list(one = quote(x))
    )
    expect_equal(res2, quote(x))
  })

  it("actually caches", {
    rand <- metaReactive({
      !!runif(1)
    })

    x1 <- withMetaMode(metaExpr(!!rand()))
    x2 <- withMetaMode(metaExpr(!!rand()))
    expect_identical(x1, x2)

    y1 <- rand()
    y2 <- rand()
    expect_identical(y1, y2)
  })

  it("has clean pipeline stages", {
    x1 <- expandCode(!!one() + 2)
    expect_equal(x1, quote(1 + 2))

    x2 <- expandCode(!!one() %>% print())
    expect_equal(x2, quote(1 %>% print()))
  })

  it("doesn't apply patchCalls at the top level", {
    x1 <- expandCode({
      one <- !!one()
      two <- !!two()
    }, patchCalls = list(one = quote(one)))
    expect_equal(x1, quote({one <- 1; two <- one}))
  })
}))
