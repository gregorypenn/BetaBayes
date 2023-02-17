#' Confidence interval of a beta distribution
#' @param shape1 Shape parameter passed to beta inverse cumulative distribution function qbeta.
#' @param shape2 Shape parameter passed to beta inverse cumulative distribution function qbeta.
#' @param CImass Central probability mass.
#' @return A numeric vector of the two quantiles bounding the central probability mass.
#' @examples
#' betaCI(6, 4)
#' betaCI(2, 50, 0.75)
#' @export
betaCI <- function(shape1, shape2, CImass = 0.95) {
  return(HDIofICDF(stats::qbeta, shape1 = shape1, shape2 = shape2))
}
