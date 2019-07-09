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

is_zip_archive <- function(x) {
  inherits(x, "pending_zip_archive")
}

archive_basedir <- function(x) {
  stopifnot(is_zip_archive(x))

  x[["basedir"]]
}

print.pending_zip_archive <- function(x, ...) {
  stopifnot(is_zip_archive(x))

  basedir <- archive_basedir(x)
  cat("Archive: ", basedir, "\n", sep = "")
  contents <- list_items(x)
  paths <- fs::path(basedir, contents)
  dirs <- fs::is_dir(paths)
  cat(paste0(rlang::rep_along(contents, "- "), contents, ifelse(dirs, "/", ""), pretty_file_sizes(paths, "  (", ")")), sep = "\n")
  invisible(x)
}

# Turn file paths into "10 B", "1.3 GiB", etc. Directories come back as "".
pretty_file_sizes <- function(paths, prefix = "", suffix = "") {
  if (length(paths) == 0) {
    return(character(0))
  }
  sizes <- fs::file_size(paths)
  ifelse(is.na(sizes) | fs::is_dir(paths),
    "",
    paste0(prefix,
      vapply(sizes, function(size) {
        format(structure(size, class = "object_size"), units = "auto", standard = "IEC")
      }, character(1)),
      suffix)
  )
}

add_items <- function(x, ...) {
  stopifnot(is_zip_archive(x))

  include_files <- rlang::dots_list(..., .homonyms = "last", .check_assign = TRUE)

  if (is.null(names(include_files))) {
    names(include_files) <- as.character(include_files)
  }

  mapply(names(include_files), include_files, FUN = function(to, from) {
    if (nchar(from) == 0) {
      from <- to
    }
    add_item(x, from, to)
    NULL
  })

  x
}

add_item <- function(x, source_file, target_file) {
  stopifnot(is_zip_archive(x))

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
      # If source is a file, but target is a directory name, ensure
      # that the file gets copied into the target, rather than as
      # the target. Without this line, fs::file_copy would treat
      # the target as a filename (it would strip off the slash).
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
  stopifnot(is_zip_archive(x))

  basedir <- archive_basedir(x)
  fs::path_rel(
    fs::dir_ls(basedir, recurse = TRUE),
    basedir
  )
}

build_archive <- function(x, output_file) {
  stopifnot(is_zip_archive(x))

  basedir <- archive_basedir(x)

  olddir <- getwd()
  setwd(basedir)
  on.exit(setwd(olddir))

  utils::zip(fs::path_abs(output_file, olddir), ".")
  invisible(output_file)
}
