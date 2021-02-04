library(shiny)

describe("expansion", isolate({
  one <- metaReactive({
    1
  })
  two <- metaReactive({
    ..(one())
  })

  it("basically works", {
    res <- withMetaMode(
      metaExpr(..(two()))
    )
    q1 <- quote(1)
    expect_equal(unclass(res), q1)
    expect_true(formatCode(res) == "1")
  })

  # NOTE this used to cache in meta mode but with expandChain it no longer
  # does, since fetching code can have side effects
  it("metaMode doesn't cache in meta mode only", {
    rand <- metaReactive({
      ..(runif(1))
    })

    x1 <- withMetaMode(metaExpr(..(rand())))
    x2 <- withMetaMode(metaExpr(..(rand())))
    expect_true(!identical(x1, x2))

    y1 <- rand()
    y2 <- rand()
    expect_identical(y1, y2)
  })

  it("has clean pipeline stages", {
    x1 <- metaReactive({ ..(one()) + 2 })
    expect_true(withMetaMode(x1()) == quote(1 + 2))

    x2 <- metaReactive({ ..(one()) %>% print() })
    expect_true(withMetaMode(x2()) == quote(1 %>% print()))
  })

}))

expect_equal_call <- function(actual, expected) {
  if (inherits(actual, "shinyMetaExpr")) {
    actual <- unclass(actual)
  }
  expect_equal(actual, expected)
}

test_that("mixed mode", isolate({
  # A bunch of different kinds of metaReactive objects that should all
  # yield quote(1+1) in meta mode.
  srcs <- list(
    metaReactive(1 + 1, inline = TRUE),
    metaReactive2(metaExpr(1 + 1), inline = TRUE),
    metaObserve(1 + 1),
    metaObserve2(metaExpr(1 + 1)),
    metaRender(renderText, 1 + 1),
    metaRender2(renderText, metaExpr(1 + 1))
  )

  # Try this scenario with each of the different kinds of objects.
  lapply(srcs, function(src) {

    mr <- metaReactive(..(src()), inline = TRUE)
    expect_equal_call(withMetaMode(mr()), quote(1 + 1))

    v <- reactiveVal(0) # cache busting reactive val
    mr2 <- metaReactive2({
      v()
      if (inherits(src, "shinymeta_observer")) {
        expect_error(src())
      } else {
        expect_identical(as.character(src()), "2")
      }
      withMetaMode(src())
    })
    expect_equal_call(withMetaMode(mr2()), quote(1 + 1))
    # Cached
    expect_equal_call(withMetaMode(mr2()), quote(1 + 1))


    # Test nesting deeper than one level

    v(isolate(v()) + 1) # bust cache for mr2
    mr3 <- metaReactive({
      ..(mr2())
    })
    expect_equal_call(withMetaMode(mr3()), quote(1 + 1))


    # Test observer
    v(isolate(v()) + 1) # bust cache for mr2
    mr4 <- metaObserve(..(src()))
    expect_equal_call(withMetaMode(mr4()), quote(1 + 1))
    mr4$destroy()  # Otherwise throws errors on next flushReact

    # Test renderer
    v(isolate(v()) + 1) # bust cache for mr2
    mr5 <- metaRender(renderText, ..(src()))
    expect_equal_call(withMetaMode(mr5()), quote(1 + 1))
  })
}))
