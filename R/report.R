#' @export
build_report <- function(report_template, output_file, vars = list(),
  include_files = list(), render = TRUE, render_args = list()
) {

  x <- zip_archive()

  # TODO: Replace knit_expand with a version that doesn't allow arbitrary
  # R expressions and doesn't search the parent frame
  rmd_source <- do.call(knitr::knit_expand, c(list(report_template), vars))

  dest_filename <- rmd_template_rename(report_template)

  dest_filename_full <- fs::path(archive_basedir(x), dest_filename)

  # TODO: Verify UTF-8 encoding is preserved
  writeLines(rmd_source, dest_filename_full)

  if (is.null(names(include_files))) {
    names(include_files) <- as.character(include_files)
  }

  mapply(names(include_files), include_files, FUN = function(from, to) {
    if (nchar(from) == 0) {
      from <- to
    }
    add_item(x, from, to)
    NULL
  })

  if (render) {
    # TODO: Perform in separate process
    do.call(rmarkdown::render, c(list(dest_filename_full), render_args), quote = TRUE)
  }

  build_archive(x, output_file)
}

# /foo/report.Rmd.in => report.Rmd
# /foo/report.Rmd => report.Rmd
# /foo/report => report.Rmd
# /foo/report.foo.bar.Rmd.in => report.foo.bar.Rmd
# /foo/report.foo.bar.Rmd => report.foo.bar.Rmd
rmd_template_rename <- function(report_template) {
  filename <- fs::path_ext_remove(fs::path_file(report_template))
  if (tolower(fs::path_ext(filename)) == "rmd") {
    filename
  } else {
    paste0(filename, ".Rmd")
  }
}
