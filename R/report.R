# TODO: Unit tests for this whole file

#' Produce a zip bundle of code and results
#'
#' @param code A language object.
#' @param output_zip_path A filename for the resulting zip bundle.
#' @param script_name A name for the R script in the zip bundle.
#' @param include_files A named list consisting of additional files that should
#'   be included in the zip bundle. The element names indicate the destination
#'   path within the bundle, specified as a relative path; the element values
#'   indicate the path to the actual file currently on disk, specified as either
#'   a relative or absolute path.
#' @param render Whether or not to call [rmarkdown::render()] on the R script.
#' @param render_args Arguments to provide to [rmarkdown::render()].
#'
#' @export
buildScriptBundle <- function(code = NULL, output_zip_path, script_name = "script.R",
  include_files = list(), render = TRUE, render_args = list()) {

  progress <- make_progress()
  progress$set(value = 0)
  progress$set(message = "Generating code")

  if (is.language(code)) {
    code <- paste(formatCode(code), collapse = "\n")
  }

  build_bundle(code, script_name, output_zip_path,
    include_files = include_files, render = render,
    render_args = render_args, progress = progress)
}


#' @param report_template Filename of an Rmd template to be expanded by [knitr::knit_expand()].
#' @param vars A named list of variables passed along to `...` in [knitr::knit_expand()].
#' @export
#' @rdname buildScriptBundle
#' @seealso knitr::knit_expand
#'
buildRmdBundle <- function(report_template, output_zip_path, vars = list(),
  include_files = list(), render = TRUE, render_args = list()) {

  force(report_template)
  force(vars)

  progress <- make_progress()
  progress$set(value = 0)
  progress$set(message = "Generating code")

  if (is.list(vars)) {
    vars <- lapply(vars, function(x) {
      if (is.language(x)) {
        paste(formatCode(x), collapse = "\n")
      } else {
        x
      }
    })
  }

  progress$set(value = 0.1)
  progress$set(message = "Expanding Rmd template")

  rmd_source <- do.call(knit_expand_safe, c(list(report_template), vars))
  rmd_filename <- template_rename(report_template, "Rmd")

  build_bundle(rmd_source, rmd_filename, output_zip_path,
    include_files = include_files, render = render,
    render_args = render_args, progress = progress)
}

build_bundle <- function(input_src, input_filename, output_zip_path,
  include_files = list(), render = TRUE, render_args = list(), progress) {

  force(input_src)
  force(input_filename)
  force(output_zip_path)
  force(include_files)
  force(render)
  force(render_args)

  # TODO: validate args
  progress$set(value = 0.2)
  progress$set(message =  "Adding items to zip archive")

  x <- zip_archive()

  dest_filename_full <- fs::path(archive_basedir(x), input_filename)

  # TODO: Verify UTF-8 encoding is preserved
  writeLines(input_src, dest_filename_full)

  add_items(x, !!!include_files)

  progress$set(value = 0.3)

  if (render) {
    progress$set(message =  "Rendering report")
    render_with_args(dest_filename_full, render_args)
  }


  progress$set(value = 0.9)
  progress$set(message =  "Compressing bundle")
  archive <- build_archive(x, output_zip_path)
  progress$set(value = 1)
  progress$close()
  archive
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

# Create shiny::Progress if possible, otherwise a dummy progress object
make_progress <- function(...) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    shiny::Progress$new(session = session, ...)
  } else {
    # Return a dummy progress object
    nothing <- function(...) {}
    list(
      set = nothing,
      inc = nothing,
      getMin = nothing,
      getMax = nothing,
      getValue = nothing,
      close = nothing,
      clone = nothing
    )
  }
}
