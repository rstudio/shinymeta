.onLoad <- function(libname, pkgname) {
  # Turn on this top secret feature of Shiny! Shhhhh!
  options(shiny.allowoutputreads = TRUE)

  message(
    "Thanks for trying out shinymeta! This package is currently\n*experimental* and will definitely have breaking changes in the near\nfuture; please use caution, especially when upgrading to a newer\nshinymeta build. https://github.com/rstudio/shinymeta/blob/master/NEWS.md"
  )
}
