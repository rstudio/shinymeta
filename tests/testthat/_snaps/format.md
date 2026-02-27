# auto-localized expressions / without assignment

    local({
      a <- 1 + 1
      if (T) {
        return("b")
      }
      a + 1
    })

# auto-localized expressions / with assignment

    mr <- local({
      a <- 1 + 1
      if (T) {
        return("b")
      }
      a + 1
    })
    mr

# auto-localized expressions / with chaining

    mr <- local({
      a <- 1 + 1
      if (T) {
        return("b")
      }
      a + 1
    })
    mr2 <- mr + 1
    mr2

# auto-localized expressions / with anonymous functions

    unlist(lapply(1:5, function(x) {
      if (x == 2) {
        return(x)
      }
    }))

# auto-localized expressions / with already localized expression

    local({
      a <- 1
      a + 2
    })

# bindToReturn / single assign works

    a <- 1 + 1
    b <- a + 1
    mr <- b + 1

# bindToReturn / double assign works

    a <- 1 + 1
    b <- a + 1
    mr <- b + 1
    a <- 1 + 1
    b <- a + 1
    mr2 <- b + 1
    mrx <- mr + mr2
    mrx

# bindToReturn / doesn't bind on local

    mr <- local({
      a <- 1 + 1
      b <- a + 1
      b + 1
    })
    mr

