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

# metaAction: unquotes properly

    x <- TRUE

---

    mr <- FALSE
    y <- mr

# metaAction: can contain code that uses !!

    foo <- 1
    x <- rlang::expr(!!foo)

