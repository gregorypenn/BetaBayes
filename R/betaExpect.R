#' Expectation of a beta distribution
#' @param shape1 Shape parameter for beta distribution.
#' @param shape2 Shape parameter for beta distribution.
#' @export
betaExpect <- function (shape1, shape2) return(shape1 / (shape1 + shape2))
