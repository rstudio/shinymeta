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

describe("expandObjects", isolate({
  output <- list()

  one <- metaReactive({
    1
  })
  two <- metaReactive({
    "# This is a comment"
    !!one()
  })
  `obs with tricky name` <- metaObserve({
    str(!!two())
  })
  output$plot <- metaRender(renderPlot, {
    ggplot(diamonds, aes(carat, price)) + geom_point()
  })

  it("works with bare names", {
    x1 <- expandObjects(
      one,
      two,
      "# top-level comment",
      `obs with tricky name`,
      output$plot
    )
    x2 <- expandObjects(
      one,
      two,
      "# top-level comment",
      `obs with tricky name`,
      output[["plot"]]
    )
    x3 <- expandCode({
      one <- !!one()
      two <- !!two()
      "# top-level comment"
      !!`obs with tricky name`()
      !!output$plot()
    }, patchCalls = list(one = quote(one), two = quote(two)))
    expect_equal(x1, x2)
    expect_equal(x2, x3)
  })

  it("works with calls", {
    x1 <- expandObjects(one(), two(), `obs with tricky name`(), output$plot())
    x2 <- expandCode({
      one <- !!one()
      two <- !!two()
      !!`obs with tricky name`()
      !!output$plot()
    }, patchCalls = list(one = quote(one), two = quote(two)))
    expect_equal(x1, x2)
  })

  it("respects explicit names", {
    x1 <- expandObjects(uno = one, dos = two, tres = `obs with tricky name`, cuatro = output$plot)
    x2 <- expandCode({
      uno <- !!one()
      dos <- !!two()
      tres <- !!`obs with tricky name`()
      cuatro <- !!output$plot()
    }, patchCalls = list(one = quote(uno), two = quote(dos)))
    expect_equal(x1, x2)
  })

  it("fails when appropriate", {
    expect_error(expandObjects(blah), "not found") # object that doesn't exist
    expect_error(expandObjects(output$table), "Could not find") # output that doesn't exist
    expect_error(expandObjects({ !!one() }), "requires all arguments") # output that doesn't exist
  })
}))

describe("make_assign_expr", {
  it("validates arguments", {
    # Invalid lhs
    expect_error(make_assign_expr(10, TRUE))
    expect_error(make_assign_expr(TRUE, TRUE))
    expect_error(make_assign_expr(NULL, TRUE))
    expect_error(make_assign_expr(character(0), TRUE))
    expect_warning(make_assign_expr(letters, TRUE))
  })

  it("works", {
    expect_identical(
      make_assign_expr("", 10),
      10
    )
    expect_identical(
      make_assign_expr("a", 10),
      quote(a <- 10)
    )
    expect_identical(
      make_assign_expr(quote(a), 10),
      quote(a <- 10)
    )
    expect_identical(
      make_assign_expr("foo$bar", 10),
      quote(`foo$bar` <- 10)
    )
    expect_identical(
      make_assign_expr(quote(foo$bar), quote(foo[["baz"]])),
      quote(foo$bar <- foo[["baz"]])
    )
  })
})
