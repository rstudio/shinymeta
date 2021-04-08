# doesn't break metaprogramming with quosures

    ~ {
      local_x
    }

---

    ~ {
      local_x
    }

---

    (~ {
      local_x
    }) * -1L

