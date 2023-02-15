#' Beta shape parameters from mean and effective sample size.
#' @param mean Expected value of the beta distribution.
#' @param n Sum of shape parameters, i.e. effective sample size.
#' @export
betaParamsMeanAndN <- function(mean, n) {
  alpha <- mean * n
  beta <- (1 - mean) * n
  return(c(alpha, beta))
}
