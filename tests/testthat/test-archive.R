context("archive")

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
