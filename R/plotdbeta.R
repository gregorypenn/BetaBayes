#' Plot a beta density distribution
#' @param shape1 Shape parameter passed to dbeta.
#' @param shape2 Shape parameter passed to dbeta.
#' @param expectation Include line for expected value.
#' @param HDImass Mass of credible interval to shade. NULL to omit.
#' @export
plotdbeta <- function (shape1, shape2, expectation = TRUE, HDImass = 0.95) {
  df <- beta_denscurve(shape1, shape2)
  plt <- ggplot2::ggplot(df, ggplot2::aes(x = p, y = density))
  if (!is.null(HDIwidth)) {
    HDI <- HDIofICDF(qbeta, shape1, shape2, credMass = HDImass)
    plt <- plt + geom_betaArea(HDI[1], HDI[2])
  }
  if (expectation) plt <- plt + geom_betaExpectation(shape1, shape2)
  return(plt + geom_path())
}
