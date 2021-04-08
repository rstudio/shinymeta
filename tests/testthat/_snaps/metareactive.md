# doesn't break metaprogramming with quosures

    # A comment in a quosure
    local_x

---

    # A comment in a quosure
    local_x

---

    {
      # A comment in a quosure
      local_x
    } * -1L

---

    if ({
      # A comment in a quosure
      local_x
    } == 123L) {
      "ok"
    }

---

    result1 <<- {
      if ({
        # A comment in a quosure
        local_x
      } == 123L) {
        "ok"
      }
    }

---

    result2 <<- {
      if ({
        # A comment in a quosure
        local_x
      } == 123L) {
        "ok"
      }
    }

