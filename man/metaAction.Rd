% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metareactive.R
\name{metaAction}
\alias{metaAction}
\title{Run/capture non-reactive code for side effects}
\usage{
metaAction(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{expr}{A code expression that will immediately be executed (before the
call to \code{metaAction} returns), and also stored for later retrieval (i.e.
meta mode).}

\item{env}{An environment.}

\item{quoted}{Is the expression quoted? This is useful when you want to use an expression
that is stored in a variable; to do so, it must be quoted with \code{\link[=quote]{quote()}}.}
}
\value{
A function that, when called in meta mode (i.e. inside
\code{\link[=expandChain]{expandChain()}}), will return the code in quoted form. If this function is
ever called outside of meta mode, it throws an error, as it is definitely
being called incorrectly.
}
\description{
Most apps start out with setup code that is non-reactive, such as
\code{\link[base:library]{library()}} calls, loading of static data into local
variables, or \code{\link[base:source]{source}}-ing of supplemental R scripts.
\code{metaAction} provides a convenient way to run such code for its side effects
(including declaring new variables) while making it easy to export that code
using \code{\link[=expandChain]{expandChain()}}. Note that \code{metaAction} executes code directly in the
\code{env} environment (which defaults to the caller's environment), so any local
variables that are declared in the \code{expr} will be available outside of
\code{metaAction} as well.
}
\examples{

setup <- metaAction({
  library(stats)

  "# Set the seed to ensure repeatable randomness"
  set.seed(100)

  x <- 1
  y <- 2
})

# The action has executed
print(x)
print(y)

# And also you can emit the code
expandChain(
  setup()
)

}
