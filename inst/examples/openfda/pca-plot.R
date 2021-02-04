library(plotly)

#' @param pca a fitted pca from princomp()
#' @param dims the number of PC to visualize (2 or 3)
#' @param drugs a character vector of drug names to highlight. The names,
#' if provided, are used to color the relevant markers.
#' @param title plot title

plot_pca <- function(pca, dims = 2, drugs = NULL, base_col = "black", base_size = 7, title = "") {
  d <- data.frame(
    PC1 = pca$scores[,1],
    PC2 = pca$scores[,2],
    PC3 = pca$scores[,3],
    drug = row.names(pca$scores)
  )
  if (dims != 2 && dims != 3) stop ("dim must be 2 or 3")

  cols <- rep(base_col, nrow(d))
  sizes <- rep(base_size, nrow(d))
  for (i in seq_along(drugs)) {
    drug <- drugs[i]
    idx <- d[["drug"]] %in% drug
    cols[idx] <- names(drug)
    sizes[idx] <- base_size * 3
  }

  add_points <- function(...) {
    if (dims == 3) add_markers(..., z = ~PC3) else add_markers(...)
  }

  p <- plot_ly(
    d, x = ~PC2, y = ~PC1, source = "pca",
    marker = list(color = cols, size = sizes, line = list(color = "transparent")),
    customdata = ~drug, text = ~drug, hoverinfo = "text",
    alpha = if (dims == 2) 0.5 else 1
  ) %>%
    add_points() %>%
    layout(
      font = list(size = 12, family = "HelveticaNeue"),
      title = title,
      showlegend = FALSE
    ) %>%
    config(displayModeBar = FALSE)

  if (dims == 2) {
    p <- layout(p, xaxis = list(scaleanchor = "y"))
  }

  p
}

