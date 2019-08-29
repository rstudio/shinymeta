.onLoad <- function(libname, pkgname) {
  # Turn on this top secret feature of Shiny! Shhhhh!
  options(shiny.allowoutputreads = TRUE)

  message(
    "********************************************************************\n",
    "* Thanks for trying out shinymeta! This package is currently       *\n",
    "* _experimental_ and the API is still evolving. Please use         *\n",
    "* caution, especially when upgrading to a newer shinymeta          *\n",
    "* build. https://github.com/rstudio/shinymeta/blob/master/NEWS.md  *\n",
    "********************************************************************"
  )

  registerMethods(list(
    # c(package, genname, class)
    c("knitr", "knit_print", "shinyMetaExpr")
  ))
}

# https://github.com/rstudio/htmltools/blob/cb452a837/R/tags.R#L22-L57
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}
