#' Add a vertical line for expected value under beta density curve.
#' @param shape1 Shape parameter passed to dbeta.
#' @param shape2 Shape parameter passed to dbeta.
#' @param linetype Linetype passed to ggplot2.
geom_betaExpectation <- function (shape1, shape2, linetype = "dashed") {
  expectation <- betaExpect(shape1, shape2)
  expectationDens <- stats::dbeta(expectation, shape1, shape2)
  ggplot2::geom_linerange(ggplot2::aes(x = expectation,
                                       ymin = 0,
                                       ymax = expectationDens),
                          linetype = linetype)
}
