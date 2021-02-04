describe(
  "auto-localized expressions", isolate({

    mr <- metaReactive({
      a <- 1 + 1
      if (T) return("b")
      a + 1
    })

    it("without assignment", {
      expect_snapshot_output(cran = TRUE, withMetaMode(mr()))
    })

    it("with assignment", {
      expect_snapshot_output(cran = TRUE, expandChain(mr()))
    })

    it("with chaining", {
      mr2 <- metaReactive({
        ..(mr()) + 1
      })

      expect_snapshot_output(cran = TRUE, expandChain(mr2()))
    })

    it("with anonymous functions", {
      mrx <- metaReactive({
        unlist(lapply(1:5, function(x) { if (x == 2) return(x) }))
      })

      expect_snapshot_output(cran = TRUE, withMetaMode(mrx()))
    })

    it("with already localized expression", {
      mrl <- metaReactive({
        local({
          a <- 1
          a + 2
        })
      })

      expect_snapshot_output(cran = TRUE, withMetaMode(mrl()))
    })

  })

)



describe(
  "bindToReturn", isolate({

    mr <- metaReactive(bindToReturn = TRUE, {
      a <- 1 + 1
      b <- a + 1
      b + 1
    })

    it("single assign works", {

      expect_snapshot_output(cran = TRUE, expandChain(invisible(mr())))
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

      expect_snapshot_output(cran = TRUE, expandChain(mrx()))
    })

    it("doesn't bind on local", {

      # TODO: maybe this should throw a warning?
      mr <- metaReactive({
        a <- 1 + 1
        b <- a + 1
        b + 1
      }, local = TRUE, bindToReturn = TRUE)

      expect_snapshot_output(cran = TRUE, expandChain(mr()))

    })


  })

)
