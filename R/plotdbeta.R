#' Plot a beta density distribution
#' @param shape1 Shape parameter passed to dbeta.
#' @param shape2 Shape parameter passed to dbeta.
#' @param expectation Include line for expected value.
#' @param HDImass Mass of credible interval to shade. NULL to omit.
#' @param xlim Limit range of x-axis.
#' @export
plotdbeta <- function (shape1, shape2, expectation = TRUE, HDImass = 0.95,
                       xlim = c(0, 1)) {
  df <- betaDensCurve(shape1, shape2)
  plt <- ggplot2::ggplot(df, ggplot2::aes(.data[["p"]], .data[["density"]]))
  if (!is.null(xlim)) plt <- plt + ggplot2::coord_cartesian(xlim = xlim)
  if (!is.null(HDImass)) {
    HDI <- HDIofICDF(stats::qbeta, shape1 = shape1, shape2 = shape2, credMass = HDImass)
    plt <- plt + geom_betaArea(HDI[1], HDI[2])
  }
  if (expectation) plt <- plt + geom_betaExpectation(shape1, shape2)
  return(plt + ggplot2::geom_path())
}
