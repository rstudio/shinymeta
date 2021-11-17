# 0.2.0.3

Small patch release to accommodate for changes made to `base::deparse()` in the next upcoming R release. (#107)

# 0.2.0.2

Small patch release to accommodate for changes made in testthat 3.1.0. (#102) 

# 0.2.0.1

Small patch release to address unit test failures on Solaris. (#101)

# 0.2.0

## Breaking changes

* A different operator, `..()` (instead of `!!`), is now expanded in meta-mode. In normal execution, this operator is not expanded, and is, instead stripped (i.e., `.,(data())` becomes `data()`). See [this wiki page](https://github.com/rstudio/shinymeta/wiki/Syntax-changes-for-shinymeta-0.2.0) for more information. ([#59](https://github.com/rstudio/shinymeta/pull/59))

## New features

* New `metaAction` function, intended for executing code for its side effects while also capturing the source for code generation. This is useful for app setup code, such as `library()` calls, `source`-ing of supplemental .R files, loading static data sets, etc. ([#71](https://github.com/rstudio/shinymeta/pull/71))

## Known issues

* `bquote(splicing = TRUE)` can't be used inside a `metaExpr()` context since the `..()` operator is reserved for `{shinymeta}`'s quasi-quotation rules. Use `{rlang}`'s `!!!` operator for splicing instead of `bquote()`.

* `metaRender()` will throw a warning about deprecated `env`/`quoted` arguments when `shiny::devmode(TRUE)`. This warning may be safely ignored and will be fixed in a future version.

# 0.1.0 (unreleased)

* Initial version, as presented at useR 2019.
