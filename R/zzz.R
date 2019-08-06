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
}
