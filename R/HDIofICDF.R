# From Kruschke 2011

#' Highest Density Interval (HDI) of Inverse Cumulative Density Function (ICDF)
#'
#' @param ICDFname R's name for the inverse cumulative density function of the distribution.
#' @param credMass The desired mass of the HDI region.
#' @param tol Passed to R's optimize function.
#' @return Highest density interval (HDI) limits in a vector.
#' @example
#' HDIofICDF(qbeta, shape1 = 30, shape2 = 12)
HDIofICDF <- function (ICDFname, credMass = 0.95, tol = 1e-8, ...) {
  incredMass <- 1.0 - credMass
  intervalWidth <- function (lowTailPr, ICDFname, credMass, ...) {
    ICDFname( credMass + lowTailPr, ...) - ICDFname( lowTailPr, ...)
  }
  optInfo <- optimize(intervalWidth, c(0, incredMass), ICDFname, credMass = credMass, tol = tol, ...)
  HDIlowTailPr <- optInfo$minimum
  return(c(ICDFname(HDIlowTailPr, ...),
           ICDFname(credMass + HDIlowTailPr, ...)))
}