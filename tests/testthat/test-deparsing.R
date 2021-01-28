describe("deparsing", isolate({

  it("escapes strings", {
    mr <- metaReactive({"foo"})
    out <- withMetaMode(mr())
    expect_equal(as.character(out), "\"foo\"")
    expect_equal(format(out), "\"foo\"")

    skip_if_not_installed("knitr")
    expect_equal(
      knitr::knit_expand(text = "a <- {{out}}", out = out),
      "a <- \"foo\""
    )
  })

  it("deparses code objects", {
    mr <- metaReactive({"foo" + 1})
    out <- withMetaMode(mr())
    expect_equal(as.character(out), "\"foo\" + 1")
    expect_equal(format(out), "\"foo\" + 1")

    skip_if_not_installed("knitr")
    expect_equal(
      knitr::knit_expand(text = "a <- {{out}}", out = out),
      "a <- \"foo\" + 1"
    )
  })

  it("deparses R objects", {
    mr <- metaReactive({list(a = 1)})
    out <- withMetaMode(mr())
    expect_equal(as.character(out), "list(a = 1)")
    expect_equal(format(out), "list(a = 1)")

    skip_if_not_installed("knitr")
    expect_equal(
      knitr::knit_expand(text = "a <- {{out}}", out = out),
      "a <- list(a = 1)"
    )
  })

}))
