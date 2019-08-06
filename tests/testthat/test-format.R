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
      expect_code_string(withMetaMode(mr()), expected)
    })

    it("with assignment", {
      expected <- c(
        'mr <- local({',
        '  a <- 1 + 1',
        '  if (T) {',
        '    return("b")',
        '  }',
        '  a + 1',
        '})',
        'mr'
      )
      expect_code_string(expandChain(mr()), expected)
    })

    it("with chaining", {
      mr2 <- metaReactive({
        ..(mr()) + 1
      })

      expect_code_string(
        expandChain(mr2()),
        c(
          'mr <- local({',
          '  a <- 1 + 1',
          '  if (T) {',
          '    return("b")',
          '  }',
          '  a + 1',
          '})',
          'mr2 <- mr + 1',
          'mr2'
        )
      )
    })

    it("with anonymous functions", {
      mrx <- metaReactive({
        unlist(lapply(1:5, function(x) { if (x == 2) return(x) }))
      })

      expect_code_string(
        withMetaMode(mrx()),
        c(
          'unlist(lapply(1:5, function(x) {',
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
        withMetaMode(mrl()),
        c(
          'local({',
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
        expandChain(invisible(mr())),
        c(
          'a <- 1 + 1',
          'b <- a + 1',
          'mr <- b + 1'
        )
      )
    })

    it("double assign works", {

      mr2 <- metaReactive({
        a <- 1 + 1
        b <- a + 1
        b + 1
      }, bindToReturn = TRUE)

      mrx <- metaReactive({
        ..(mr()) + ..(mr2())
      })

      expect_code_string(
        expandChain(mrx()),
        c(
          'a <- 1 + 1',
          'b <- a + 1',
          'mr <- b + 1',
          'a <- 1 + 1',
          'b <- a + 1',
          'mr2 <- b + 1',
          'mrx <- mr + mr2',
          'mrx'
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
        expandChain(mr()),
        c(
          'mr <- local({',
          '  a <- 1 + 1',
          '  b <- a + 1',
          '  b + 1',
          '})',
          'mr'
        )
      )

    })


  })

)
