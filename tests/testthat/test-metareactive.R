test_that("doesn't break metaprogramming with quosures", {
  isolate({

    my_quo <- local({
      local_x <- 123L
      rlang::quo({ local_x })
    })

    r1 <- metaReactive({my_quo}, quoted = TRUE, varname = "r1")
    r2 <- rlang::inject(metaReactive(!!my_quo, varname = "r2"))
    r3 <- rlang::inject(metaReactive(!!my_quo * -1L, varname = "r3"))

    expect_identical(r1(), 123L)
    expect_identical(r2(), 123L)
    expect_identical(r3(), -123L)

    expect_snapshot_output(formatCode(withMetaMode(r1())))
    expect_snapshot_output(formatCode(withMetaMode(r2())))
    expect_snapshot_output(formatCode(withMetaMode(r3())))
  })
})
