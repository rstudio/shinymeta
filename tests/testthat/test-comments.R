library(shiny)

capturePrint <- function(x) {
  capture.output(print(x))
}

describe("metaReactive", isolate({

  it("works", {
    mr <- metaReactive({
      "# a comment"
      1 + 1
    })
    expected_output <- c("# a comment", "1 + 1")
    actual_output <- capturePrint(withMetaMode(mr()))
    expect_equal(expected_output, actual_output)
  })

  it("works with metaReactive2", {
    mr2 <- metaReactive2({
      4 + 4
      metaExpr({
        "# a comment"
        1 + 1
      })
    })
    expected_output <- c("# a comment", "mr2 <- 1 + 1")
    actual_output <- capturePrint(expandChain(invisible(mr2())))
    expect_equal(expected_output, actual_output)
  })

  it("throws warning if mis-specified", {
    mrw <- metaReactive({
      1 + 1
      "# not a comment"
    })
    expected_output <- c("1 + 1", "\"# not a comment\"")
    actual_output <- expect_warning(
      capturePrint(withMetaMode(mrw())),
      "comment can not appear as the last child"
    )
    expect_equal(expected_output, actual_output)
  })

}))


describe("metaObserve", isolate({

  it("works", {
    mo <- metaObserve({
      "# a comment"
      1 + 1
    })
    expected_output <- c("# a comment", "1 + 1")
    actual_output <- capturePrint(expandChain(mo()))
    expect_equal(expected_output, actual_output)
  })

  it("works with metaObserve2", {
    mo2 <- metaObserve2({
      4 + 4
      metaExpr({
        "# a comment"
        1 + 1
      })
    })
    expected_output <- c("# a comment", "1 + 1")
    actual_output <- capturePrint(withMetaMode(mo2()))
    expect_equal(expected_output, actual_output)
  })

  it("throws warning if mis-specified", {
    mow <- metaObserve({
      1 + 1
      "# a comment"
    })
    expected_output <- c("1 + 1", "\"# a comment\"")
    actual_output <- expect_warning(
      capturePrint(withMetaMode(mow())),
      "comment can not appear as the last child"
    )
    expect_equal(expected_output, actual_output)
  })

}))


describe("metaRender", isolate({

  it("works", {
    mrt <- metaRender(renderText, {
      "# a comment"
      1 + 1
    })
    expected_output <- c("# a comment", "1 + 1")
    actual_output <- capturePrint(withMetaMode(mrt()))
    expect_equal(expected_output, actual_output)
  })

  it("works with metaRender2", {
    mrt2 <- metaRender2(renderText, {
      4 + 4
      metaExpr({
        "# a comment"
        1 + 1
      })
    })
    expected_output <- c("# a comment", "1 + 1")
    actual_output <- capturePrint(expandChain(mrt2()))
    expect_equal(expected_output, actual_output)
  })

  it("throws warning if mis-specified", {
    mrw <- metaRender(renderText, {
      1 + 1
      "# not a comment"
    })
    expected_output <- c("1 + 1", "\"# not a comment\"")
    actual_output <- expect_warning(
      capturePrint(withMetaMode(mrw())),
      "comment can not appear as the last child"
    )
    expect_equal(expected_output, actual_output)
  })

}))

describe("various edge cases", isolate({
  mr <- metaReactive({
    "# Escaped \"quotes\" should \'be' supported"
    NULL
  })
  mr2 <- metaReactive({
    '# Escaped \"quotes" should \'be\' supported'
    NULL
  })
  expected <- c(
    "# Escaped \"quotes\" should 'be' supported",
    "NULL"
  )
  expect_equal(
    capturePrint(withMetaMode(mr())), expected
  )
  expect_equal(
    capturePrint(withMetaMode(mr2())), expected
  )
  mr <- metaReactive({
    " # This shouldn't count as a comment " # Leading whitespace
    " '# This either' "                     # Nested quote
    " \"# Or this\" "                       # Nested dbl-quote
  })
  expect_equal(
    capturePrint(withMetaMode(mr())),
    c(
      deparse(" # This shouldn't count as a comment "),
      deparse(" '# This either' "),
      deparse(" \"# Or this\" ")
    )
  )
  mr <- metaReactive({
    "# This should be a comment"
    paste(
      "# But this should not",
      "be a comment"
    )
  })
  expect_equal(
    capturePrint(withMetaMode(mr())),
    c(
      "# This should be a comment",
      "paste(\"# But this should not\", \"be a comment\")"
    )
  )


  mr <- metaReactive({
    message("got here")
    "# This is not a comment"
  })

  mr2 <- metaReactive({
    ..(mr())
    NULL
  })

  out <- expect_warning(
    capturePrint(withMetaMode(mr2())),
    "comment can not appear as the last child"
  )

  expect_equal(
    out,
    c(
      "message(\"got here\")",
      "\"# This is not a comment\"",
      "NULL"
    )
  )

  x <- metaReactive({
    "# This comment should appear above the assignment"
    1 + 1
  })

  out <- capturePrint(expandChain(invisible(x())))
  expect_equal(
    out,
    c(
      "# This comment should appear above the assignment",
      "x <- 1 + 1"
    )
  )

  x2 <- metaReactive({
    "# This comment should appear above the assignment"
    ..(x()) + 1
  })
  expect_equal(
    capturePrint(expandChain(invisible(x2()))),
    c(
      "# This comment should appear above the assignment",
      "x <- 1 + 1",
      "# This comment should appear above the assignment",
      "x2 <- x + 1"
    )
  )

  # TODO: What should happen if \n appears in a string-comment?
}))
