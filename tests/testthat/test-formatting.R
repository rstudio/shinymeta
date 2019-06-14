context("localize")

expect_code_string <- function(code, expected, ...) {
  actual <- strsplit(formatCode(code, ...), "\n")[[1]]
  expect_equal(actual, expected)
}

# utility function for creating test expectations
generate_code_string <- function(code, ...) {
  cat("c(", "\n", "'")
  str <- strsplit(formatCode(code, ...), "\n")[[1]]
  cat(str, sep = "',\n'")
  cat("'", ")")
}

describe(
  "localization works", {

  mr <- metaReactive({
    a <- 1 + 1
    if (T) return("b")
    a + 1
  })

  expect_code_string(
    expandCode(!!mr()),
    c(
      'a <- 1 + 1',
      'if (T) {',
      '  return("b")',
      '}',
      'a + 1'
    )
  )

  expected <- c(
    'x <- local({',
    '  a <- 1 + 1',
    '  if (T) {',
    '    return("b")',
    '  }',
    '  a + 1',
    '})'
  )
  expect_code_string(expandCode(x <- !!mr()), expected)
  expect_code_string(expandCode({x <- !!mr()}), expected)

  expect_code_string(
    expandCode({
      mr1 <- !!mr()
      mr2 <- !!mr()
    }),
    c(
      'mr1 <- local({',
      '  a <- 1 + 1',
      '  if (T) {',
      '    return("b")',
      '  }',
      '  a + 1',
      '})',
      'mr2 <- local({',
      '  a <- 1 + 1',
      '  if (T) {',
      '    return("b")',
      '  }',
      '  a + 1',
      '})'
    )
  )

})


describe(
  "unpacking works", {

  mr <- metaReactive({
    a <- 1+1
    b <- a+1
    b+1
  })

  expect_code_string(
    expandCode({mr <- !!mr()}),
    c(
      'a <- 1 + 1',
      'b <- a + 1',
      'mr <- b + 1'
    )
  )

  expect_code_string(
    expandCode({
      mr1 <- !!mr()
      mr2 <- !!mr()
    }),
    c(
      'a <- 1 + 1',
      'b <- a + 1',
      'mr1 <- b + 1',
      'a <- 1 + 1',
      'b <- a + 1',
      'mr2 <- b + 1'
    )
  )

  expect_code_string(
    expandCode({
      mr <- !!mr()
    }),
    c(
      'a <- 1 + 1',
      'b <- a + 1',
      'mr <- b + 1'
    )
  )

})
