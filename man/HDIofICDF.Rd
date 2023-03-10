% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HDIofICDF.R
\name{HDIofICDF}
\alias{HDIofICDF}
\title{Highest Density Interval (HDI) of Inverse Cumulative Density Function (ICDF)}
\usage{
HDIofICDF(ICDFname, credMass = 0.95, tol = 1e-08, ...)
}
\arguments{
\item{ICDFname}{R's name for the inverse cumulative density function of the distribution. Generally a q function like qbeta.}

\item{credMass}{The desired mass of the HDI region.}

\item{tol}{Passed to R's optimize function.}

\item{...}{Additional arguments passed to ICDF.}
}
\value{
Highest density interval (HDI) bounds in a vector of length 2.
}
\description{
Highest Density Interval (HDI) of Inverse Cumulative Density Function (ICDF)
}
