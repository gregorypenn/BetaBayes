#' Points and densities along a beta distribution
#'
#' @param shape1 Shape parameter passed to `dbeta`.
#' @param shape2 Shape parameter passed to `dbeta`.
#' @param pmin Minimum value for density sampling.
#' @param pmax Maximum value for density sampling.
#' @param n Number of points to sample on the curve.
#' @return A dataframe with columns for beta variate p and its probability density.
#' @export
betaDensCurve <- function (shape1, shape2, pmin = 0, pmax = 1, n = 1000) {
  p <- seq(pmin, pmax, length.out = n)
  density <- stats::dbeta(p, shape1, shape2)
  return(data.frame(p, density))
}
