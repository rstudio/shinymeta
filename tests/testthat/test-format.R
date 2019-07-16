context("localize")

expect_code_string <- function(code, expected, ...) {
  actual <- unclass(formatCode(code, ...))
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
  "auto-localized expressions", isolate({

    mr <- metaReactive({
      a <- 1 + 1
      if (T) return("b")
      a + 1
    })

    it("without assignment", {
      expected <- c(
        'local({',
        '  a <- 1 + 1',
        '  if (T) {',
        '    return("b")',
        '  }',
        '  a + 1',
        '})'
      )
      expect_code_string(expandCode(!!mr()), expected)
      expect_code_string(expandCode({!!mr()}), expected)
    })

    # TODO: can we reliably do with with `=` assignment?
    it("with assignment", {
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
    })

    it("with multiple assignments", {
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

    it("with anonymous functions", {
      mrx <- metaReactive({
        unlist(lapply(1:5, function(x) { if (x == 2) return(x) }))
      })

      expect_code_string(
        expandCode(two <- !!mrx()),
        c(
          'two <- unlist(lapply(1:5, function(x) {',
          '  if (x == 2) {',
          '    return(x)',
          '  }',
          '}))'
        )
      )
    })

    it("with already localized expression", {
      mrl <- metaReactive({
        local({
          a <- 1
          a + 2
        })
      })

      expect_code_string(
        expandCode(three <- !!mrl()),
        c(
          'three <- local({',
          '  a <- 1',
          '  a + 2',
          '})'
        )
      )

    })

  })

)



describe(
  "bindToReturn", isolate({

    mr <- metaReactive({
      a <- 1 + 1
      b <- a + 1
      b + 1
    }, bindToReturn = TRUE)

    it("single assign works", {

      expect_code_string(
        expandCode({mr <- !!mr()}),
        c(
          'a <- 1 + 1',
          'b <- a + 1',
          'mr <- b + 1'
        )
      )
    })

    it("double assign works", {
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
    })

    it("doesn't bind on local", {

      # TODO: maybe this should throw a warning?
      mr <- metaReactive({
        a <- 1 + 1
        b <- a + 1
        b + 1
      }, local = TRUE, bindToReturn = TRUE)

      expect_code_string(
        expandCode({
          mr1 <- !!mr()
          mr2 <- !!mr()
        }),
        c(
          'mr1 <- local({',
          '  a <- 1 + 1',
          '  b <- a + 1',
          '  b + 1',
          '})',
          'mr2 <- local({',
          '  a <- 1 + 1',
          '  b <- a + 1',
          '  b + 1',
          '})'
        )
      )

    })


  })

)




describe(
  "stripping trivial assignment", isolate({

    it("basically works", {

      data <- metaReactive({iris})
      my_lm <- metaReactive({
        data <- !!data()
        lm(Sepal.Length ~ Sepal.Width, data = data)
      })

      expect_code_string(
        expandChain(my_lm()),
        c(
          'data <- iris',
          'my_lm <- lm(Sepal.Length ~ Sepal.Width, data = data)',
          'my_lm'
        )
      )

    })
  })

)
