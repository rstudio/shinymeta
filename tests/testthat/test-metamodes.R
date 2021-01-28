test_that("state machine is followed", {
  # Normal (FALSE)

  expect_identical(metaMode(), FALSE)

  withMetaMode({
    expect_identical(metaMode(), TRUE)
  })

  metaDispatch(
    normal = expect_identical(metaMode(), FALSE),
    meta = stop("Error in test")
  )

  metaExpr({
    expect_identical(metaMode(), FALSE)
  })


  # Meta (TRUE)
  withMetaMode(mode = TRUE, {
    expect_identical(metaMode(), TRUE)

    withMetaMode({
      expect_identical(metaMode(), TRUE)
    })

    metaDispatch(
      normal = stop("Error in test"),
      meta = expect_identical(metaMode(), "mixed")
    )

    metaExpr({
      expect_identical(metaMode(), TRUE)
    })
  })


  # Mixed ("mixed")
  withMetaMode(mode = TRUE, metaDispatch(normal = stop("Error in test"), meta = {
    expect_identical(metaMode(), "mixed")

    withMetaMode({
      expect_identical(metaMode(), TRUE)
    })

    metaDispatch(
      normal = expect_identical(metaMode(), FALSE),
      meta = stop("Error in test")
    )

    metaExpr({
      expect_identical(metaMode(), TRUE)
    })
  }))
})
