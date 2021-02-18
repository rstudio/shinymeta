template_path <- test_path("assets/template.Rmd")

test_that("buildRmdBundle works", {
  output_zip_path <- tempfile("testbundle-", fileext = ".zip")

  buildRmdBundle(template_path, output_zip_path, vars = list(
    desc = "# Weekly report\n\nLooks like `cars` hasn't changed since last week.",
    code_chunk = metaExpr(quote({plot(cars)})),
    code_inline = metaExpr(quote(1 + 1)),
    x = 1, y = 2
  ))

  working_dir <- tempfile()
  dir.create(working_dir)
  on.exit(unlink(working_dir, recursive = TRUE))

  unzip(output_zip_path, exdir = working_dir)
  expect_true(file.exists(file.path(working_dir, "template.html")))

  expect_snapshot_file(file.path(working_dir, "template.Rmd"))
})

test_that("buildRmdBundle rejects unsafe knit_expand results", {
  output_zip_path <- tempfile("testbundle-", fileext = ".zip")

  # (Begin code chunk) fails
  expect_error(
    buildRmdBundle(template_path, output_zip_path, vars = list(
      desc = "# Weekly report\n\nLooks like `cars` hasn't changed since last week.\n```{r}\nmessage('owned')\n```\n",
      code_chunk = metaExpr(quote({plot(cars)})),
      code_inline = metaExpr(quote(1 + 1)),
      x = 1, y = 2
    ))
  )

  # (End code chunk) fails
  expect_error(
    buildRmdBundle(template_path, output_zip_path, vars = list(
      desc = "# Weekly report\n\nLooks like `cars` hasn't changed since last week.\n```\n",
      code_chunk = metaExpr(quote({plot(cars)})),
      code_inline = metaExpr(quote(1 + 1)),
      x = 1, y = 2
    ))
  )

  # (Inline code) fails
  expect_error(
    buildRmdBundle(template_path, output_zip_path, vars = list(
      desc = "# Weekly report\n\nLooks like `cars` hasn't changed since last week.\n`r message('owned')`\n",
      code_chunk = metaExpr(quote({plot(cars)})),
      code_inline = metaExpr(quote(1 + 1)),
      x = 1, y = 2
    ))
  )

  # Begin/end of inline.code are in two different spots - fails
  expect_error(
    buildRmdBundle(template_path, output_zip_path, vars = list(
      desc = "# Weekly report\n\nLooks like `cars` hasn't changed since last week.\n",
      code_chunk = metaExpr(quote({plot(cars)})),
      code_inline = metaExpr(quote(1 + 1)),
      x = "`r message('owned') #", y = "`"
    ))
  )
})
