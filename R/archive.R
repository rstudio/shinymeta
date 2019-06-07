zip_archive <- function(temp_dir = NULL) {
  if (!is.null(temp_dir) && (!is.character(temp_dir) || length(temp_dir) != 1)) {
    stop("temp_dir must be a single-element character vector")
  }

  if (is.null(temp_dir)) {
    temp_dir <- tempfile("archive")
    fs::dir_create(temp_dir, mode = "u=rwx,go=")
  } else if (!dir.exists(temp_dir)) {
    stop("temp_dir directory does not exist")
  }

  structure(
    list(basedir = temp_dir),
    class = "pending_zip_archive"
  )
}

archive_basedir <- function(x) {
  x[["basedir"]]
}

print.pending_zip_archive <- function(x, ...) {
  basedir <- archive_basedir(x)
  cat("Archive: ", basedir, "\n", sep = "")
  contents <- list_items(x)
  dirs <- fs::is_dir(fs::path(basedir, contents))
  cat(paste0(rlang::rep_along(contents, "  "), contents, ifelse(dirs, "/", "")), sep = "\n")
}

add_items <- function(x, ...) {
  include_files <- rlang::dots_list(..., .homonyms = "last", .check_assign = TRUE)

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

  x
}

add_item <- function(x, source_file, target_file) {
  if (!inherits(x, "pending_zip_archive")) {
    stop("x must be return value from zip_archive")
  }
  if (!is.character(source_file) || length(source_file) != 1) {
    stop("source_file must be a single-element character vector")
  }
  if (!is.character(target_file) || length(target_file) != 1) {
    stop("target_file must be a single-element character vector")
  }

  if (fs::is_absolute_path(target_file)) {
    stop("target_file must be a relative path")
  }

  full_src <- fs::path_abs(source_file)

  basedir <- archive_basedir(x)

  if (fs::dir_exists(full_src)) {
    full_dest <- fs::path(basedir, target_file)
    fs::dir_copy(full_src, full_dest)
  } else {
    if (grepl("[/\\]$", target_file)) {
      target_file <- fs::path(target_file, fs::path_file(source_file))
    }
    full_dest <- fs::path(basedir, target_file)

    if (!fs::path_dir(target_file) %in% c("", "."))
      fs::dir_create(fs::path_dir(full_dest), recurse = TRUE)
    fs::file_copy(full_src, full_dest)
  }

  x
}

list_items <- function(x) {
  basedir <- archive_basedir(x)
  fs::path_rel(
    fs::dir_ls(basedir, recurse = TRUE),
    basedir
  )
}

build_archive <- function(x, output_file) {
  basedir <- archive_basedir(x)

  olddir <- getwd()
  setwd(basedir)
  on.exit(setwd(olddir))

  zip(fs::path_abs(output_file, olddir), ".")
  invisible(output_file)
}
