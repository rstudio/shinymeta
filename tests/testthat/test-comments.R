context("comments")
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
    expected_output <- c("# a comment", "1 + 1")
    actual_output <- capturePrint(expandCode(!!mr2()))
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
    actual_output <- capturePrint(expandCode(!!mo()))
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
    actual_output <- capturePrint(expandCode(!!mrt2()))
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
  expect_equal(
    capturePrint(expandCode({
      "# Escaped \"quotes\" should \'be' supported"
      NULL
    })),
    c(
      "# Escaped \"quotes\" should 'be' supported",
      "NULL"
    )
  )

  expect_equal(
    capturePrint(expandCode({
      '# Escaped \"quotes" should \'be\' supported'
      NULL
    })),
    c(
      "# Escaped \"quotes\" should 'be' supported",
      "NULL"
    )
  )

  expect_equal(
    capturePrint(expandCode({
      " # This shouldn't count as a comment " # Leading whitespace
      " '# This either' "                     # Nested quote
      " \"# Or this\" "                       # Nested dbl-quote
    })),
    c(
      deparse(" # This shouldn't count as a comment "),
      deparse(" '# This either' "),
      deparse(" \"# Or this\" ")
    )
  )

  expect_equal(
    capturePrint(expandCode({
      "# This should be a comment"
      paste(
        "# But this should not",
        "be a comment"
      )
    })),
    c(
      "# This should be a comment",
      "paste(\"# But this should not\", \"be a comment\")"
    )
  )


  mr <- metaReactive({
    "# This is not a comment"
  })

  mr2 <- metaReactive({
    !!mr()
    NULL
  })

  out <- expect_warning(
    capturePrint(withMetaMode(mr2())),
    "comment can not appear as the last child"
  )

  expect_equal(
    out,
    c(
      "\"# This is not a comment\"",
      "NULL"
    )
  )

  x <- metaReactive({
    "# This comment should appear above the assignment"
    1 + 1
  })

  out <- capturePrint(expandCode(x <- !!x()))
  expect_equal(
    out,
    c(
      "# This comment should appear above the assignment",
      "x <- 1 + 1"
    )
  )

  code <- expandCode({
    x <- !!x()
    x2 <- !!x()
  })
  expect_equal(
    capturePrint(code),
    c(
      "# This comment should appear above the assignment",
      "x <- 1 + 1",
      "# This comment should appear above the assignment",
      "x2 <- 1 + 1"
    )
  )

  # TODO: What should happen if \n appears in a string-comment?
}))
