# doesn't break metaprogramming with quosures

    # A comment in a quosure
    local_x

---

    {
      # A comment in a quosure
      local_x
    } * -1L

---

    result1 <<- {
      if ({
        # A comment in a quosure
        local_x
      } == 123L) {
        "ok"
      }
    }

# metaAction: basically works

    a <- 6

# metaAction: unquotes properly

    x <- TRUE

---

    mr <- FALSE
    y <- mr

