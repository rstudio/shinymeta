% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output-code.R
\name{outputCodeButton}
\alias{outputCodeButton}
\title{Overlay an icon on a shiny output}
\usage{
outputCodeButton(
  outputObj,
  label = "Show code",
  icon = shiny::icon("code"),
  width = NULL,
  ...
)
}
\arguments{
\item{outputObj}{A shiny output container (e.g., \link[shiny:plotOutput]{shiny::plotOutput}, \link[shiny:textOutput]{shiny::textOutput}, etc)}

\item{label}{The contents of the button or link--usually a text label, but
you could also use any other HTML, like an image.}

\item{icon}{An optional \code{\link[shiny:icon]{icon()}} to appear on the button.}

\item{width}{The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
see \code{\link[shiny:validateCssUnit]{validateCssUnit()}}.}

\item{...}{Named attributes to be applied to the button or link.}
}
\value{
the \code{outputObj} wrapped in a card-like HTML container.
}
\description{
Intended for overlaying a button over a shiny output, that when clicked,
displays code for reproducing that output. The button is
similar to an \code{\link[shiny:actionButton]{shiny::actionButton()}}, but instead of providing an \code{inputId},
the id is determined by the id of the \code{outputObj}. The name
of that input is a function of \code{outputObj}'s \code{outputId}:
\code{input$OUTPUTID_output_code}.
}
\examples{

if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    sliderInput("n", label = "Number of samples", min = 10, max = 100, value = 30),
    outputCodeButton(plotOutput("p"))
  )
  server <- function(input, output) {
    output$p <- metaRender(renderPlot, {
      plot(sample(..(input$n)))
    })
    observeEvent(input$p_output_code, {
      code <- expandChain(output$p())
      displayCodeModal(code)
    })
  }
  shinyApp(ui, server)
}

}
\seealso{
\link{displayCodeModal}
}
