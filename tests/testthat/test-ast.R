context("walk-ast")

test_that("walk_ast can handle missing args", {
  expr <- quote(mtcars[1:5, ])
  expect_identical(
    expr, walk_ast(expr, identity)
  )
})
