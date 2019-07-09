context("test-expandchain")

mr1 <- metaReactive({
  1
})

mr2 <- metaReactive(
  {!!mr1() + 2}
)

# Can't infer varname, but inlined
metaReactive({
  3
}, inline = TRUE) -> mr3

# Can't infer varname, explicitly provided
metaReactive({
  4
}, varname = "mrFour") -> mr4

o <- metaObserve({
  !!mr2() + !!mr3() + !!mr4()
})

describe("expandChain", {
  it("basically works", {
    x <- capture.output(print(expandChain(mr2())))
    expect_identical(x, c("mr1 <- 1", "mr2 <- mr1 + 2", "mr2"))

    x <- capture.output(print(expandChain(
      "# A comment",
      o()
    )))
    expect_identical(x, c(
      "# A comment",
      "mr1 <- 1",
      "mr2 <- mr1 + 2",
      "mrFour <- 4",
      "mr2 + 3 + mrFour"
    ))
  })

  it("can emit metaReactive invisibly", {
    x <- capture.output(print(expandChain(
      quote(library(ggplot2)),
      # NULL should be ignored
      NULL,
      # Use invisible() to cause mr2 to be defined, but not printed
      invisible(mr2())
    )))
    expect_identical(x, c(
      "library(ggplot2)",
      "mr1 <- 1",
      "mr2 <- mr1 + 2"
    ))
  })

  it("rejects bad arguments", {
    expect_error(expandChain(1))
    expect_error(expandChain(quote(1)))
    expect_error(expandChain("hi"))
    expect_error(expandChain(list()))
    expect_error(expandChain(mr)) # missing ()
    expect_error(expandChain(cars))
    expect_error(expandChain(a = NULL), "Named")
  })
})

describe("expansion context", {
  it("basically works", {
    ec <- newExpansionContext()

    x <- capture.output(print(expandChain(.expansionContext = ec,
      invisible(mr2())
    )))
    expect_identical(x, c("mr1 <- 1", "mr2 <- mr1 + 2"))

    x <- capture.output(print(expandChain(.expansionContext = ec,
      o()
    )))
    expect_identical(x, c("mrFour <- 4", "mr2 + 3 + mrFour"))
  })

  it("can substitute", {
    ec <- newExpansionContext()
    ec$substituteMetaReactive(mr2, 100 + 200)
    ec$substituteMetaReactive(mr3, 1000 + 2000)

    x <- capture.output(print(expandChain(.expansionContext = ec,
      o()
    )))
    expect_identical(x, c(
      "mr2 <- 100 + 200",
      "mrFour <- 4",
      "mr2 + (1000 + 2000) + mrFour"
    ))
  })
})
