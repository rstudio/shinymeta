describe("name translation", {
  it("strips non-.Rmd extensions", {
    test_cases <- list(
      "/foo/report.Rmd.in" = "report.Rmd",
      "/foo/report.Rmd" = "report.Rmd",
      "/foo/report" = "report.Rmd",
      "/foo/report.foo.bar.Rmd.in" = "report.foo.bar.Rmd",
      "/foo/report.foo.bar.Rmd" = "report.foo.bar.Rmd"
    )
    mapply(names(test_cases), test_cases, FUN = function(from, to) {
      expect_identical(template_rename(from), to)
    })
  })

  it("strips non-.R extensions", {
    test_cases <- list(
      "/foo/report.R.in" = "report.R",
      "/foo/report.R" = "report.R",
      "/foo/report" = "report.R",
      "/foo/report.foo.bar.R.in" = "report.foo.bar.R",
      "/foo/report.foo.bar.R" = "report.foo.bar.R"
    )
    mapply(names(test_cases), test_cases, FUN = function(from, to) {
      expect_identical(template_rename(from, "R"), to)
    })
  })
})

test_that("zip building", {
  tmp <- tempfile(pattern = "dir")
  dir.create(file.path(tmp, "foo"), recursive = TRUE)
  file1 <- file.path(tmp, "foo", "bar")
  file.create(file1)

  za <- zip_archive()

  # Copy file where dest doesn't have trailing slash
  add_items(za, baz = file1)
  expect_equal(list_items(za), fs::path(c("baz")))

  # Copy file where dest has trailing slash
  add_item(za, file1, "qux/")
  expect_equal(list_items(za), fs::path(c("baz", "qux", "qux/bar")))

  # Copy dir where dest doesn't have trailing slash
  lst <- setNames(fs::path_dir(file1), list("quuz"))
  add_items(za, !!!lst)
  expect_equal(list_items(za), fs::path(c("baz", "quuz", "quuz/bar", "qux", "qux/bar")))

  # Copy dir where dest does have trailing slash (no difference)
  lst2 <- setNames(fs::path_dir(file1), list("corge/"))
  add_items(za, !!!lst2)
  expect_equal(list_items(za), fs::path(c("baz", "corge", "corge/bar", "quuz", "quuz/bar", "qux", "qux/bar")))
})
