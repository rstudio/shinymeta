test_that("knit_expand_safe ignores calling environment", {

  foo <- "bar"
  expect_error(
    knit_expand_safe(text = "{{ foo }}"),
    "foo"
  )
  expect_identical(
    knit_expand_safe(text = "{{ foo }}", vars = list(foo = foo)),
    "bar"
  )

  # matches_before is (at the time of this writing) a local variable in the
  # knit_expand_safe, which is the parent.frame of knit_expand call. By default,
  # knit_expand would be able to "see" that variable; knit_expand_safe is
  # supposed to prevent that.
  expect_error(
    knit_expand_safe(text = "{{ matches_before }}"),
    "matches_before"
  )

  expect_identical(
    local({ x <- "whatever"; knit_expand_safe(text = "{{ x }}", vars = list(x = x)) }),
    "whatever"
  )

  expect_identical(
    knit_expand_safe(text = "{{ toupper('hello') }}"),
    "HELLO"
  )

  # Use one of knit_expand_safe's parameter names as a var
  expect_identical(
    knit_expand_safe(text = "{{ text }}", vars = list(text = "something")),
    "something"
  )
})
