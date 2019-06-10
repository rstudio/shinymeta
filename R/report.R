# TODO: Unit tests for this whole file

#' @export
build_script_bundle <- function(code = NULL, output_zip_path, script_name = "script.R",
  include_files = list(), render = TRUE, render_args = list()) {

  if (is.language(code)) {
    code <- format_tidy_code(code)
  }

  build_bundle(code, script_name, output_zip_path,
    include_files = include_files, render = render,
    render_args = render_args)
}

#' @export
build_rmd_bundle <- function(report_template, output_zip_path, vars = list(),
  include_files = list(), render = TRUE, render_args = list()) {

  force(report_template)
  force(vars)

  if (is.list(vars)) {
    vars <- lapply(vars, function(x) {
      if (is.language(x)) {
        format_tidy_code(x)
      } else {
        x
      }
    })
  }

  # TODO: Replace knit_expand with a version that doesn't allow arbitrary
  # R expressions and doesn't search the parent frame
  rmd_source <- do.call(knitr::knit_expand, c(list(report_template), vars))
  rmd_filename <- template_rename(report_template, "Rmd")

  build_bundle(rmd_source, rmd_filename, output_zip_path,
    include_files = include_files, render = render,
    render_args = render_args)
}

build_bundle <- function(input_src, input_filename, output_zip_path,
  include_files = list(), render = TRUE, render_args = list()) {

  force(input_src)
  force(input_filename)
  force(output_zip_path)
  force(include_files)
  force(render)
  force(render_args)

  # TODO: validate args

  x <- zip_archive()

  dest_filename_full <- fs::path(archive_basedir(x), input_filename)

  # TODO: Verify UTF-8 encoding is preserved
  writeLines(input_src, dest_filename_full)

  add_items(x, !!!include_files)

  if (render) {
    render_with_args(dest_filename_full, render_args)
  }

  build_archive(x, output_zip_path)
}

render_with_args <- function(input_file, render_args = list(), switch_dirs = TRUE, fork = TRUE) {

  if (switch_dirs) {
    old_wd <- getwd()
    setwd(fs::path_dir(input_file))
    on.exit(setwd(old_wd))
  }

  if (fork) {
    callr::r(
      function(...) rmarkdown::render(...),
      # https://github.com/rstudio/rmarkdown/issues/1204#issuecomment-344884823
      args = c(list(input_file, envir = globalenv()), render_args)
    )
  } else {
    do.call(rmarkdown::render, c(list(input_file), render_args), quote = TRUE)
  }
}

# /foo/report.Rmd.in => report.Rmd
# /foo/report.Rmd => report.Rmd
# /foo/report => report.Rmd
# /foo/report.foo.bar.Rmd.in => report.foo.bar.Rmd
# /foo/report.foo.bar.Rmd => report.foo.bar.Rmd
template_rename <- function(input_template, extension = "Rmd") {
  stopifnot(is.character(extension) && length(extension) == 1 && identical(TRUE, nzchar(extension)))

  filename <- fs::path_ext_remove(fs::path_file(input_template))
  if (tolower(fs::path_ext(filename)) == tolower(extension)) {
    filename
  } else {
    paste0(filename, ".", extension)
  }
}
