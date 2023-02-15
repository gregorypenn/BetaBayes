# Not yet working

#' Simulate the highest density interval (HDI) width for a beta posterior.
#'
#' @param genPriorMean The mean of the generating distribution.
#' @param genPriorN The sum of the generating distributions shape parameters.
#' @param sampleSize The number of samples.
#' @param audPriorMean The mean of the prior for analysis.
#' @param audPriorN The sum of the prior's shape parameters or sample size on which the prior is based.
#' @param CImass The desired mass of the highest density interval (HDI).
#' @return A dataframe with columns n and HDIwidth.
probOfPosteriorCIwidth <- function(genPriorMean,
                                   genPriorN,
                                   sampleSize,
                                   audPriorMean = 0.5,
                                   audPriorN = 2,
                                   CImass = 0.95) {

  # Convert prior mean and N to a, b parameter values of beta distribution
  genPriorA <- genPriorMean * genPriorN
  genPriorB <- (1.0 - genPriorMean) * genPriorN

  audPriorA <- audPriorMean * audPriorN
  audPriorB <- (1.0 - audPriorMean) * audPriorN

  # Initialize loop for incrementing sampleSize
  sampleSize <- minSamples:maxSamples
  hdiWid_expected <- numeric()
  # hdiwid_l <- list()

  zvec <- 0:sampleSize # All possible z values for N flips.

  # Compute probability of each z value for data-generating prior.
  pzvec <- exp(lchoose(sampleSize, zvec)
               + lbeta(zvec + genPriorA, sampleSize - zvec + genPriorB)
               - lbeta(genPriorA, genPriorB))

  # For each z value, compute HDI. hdiMat is min, max of HDI for each z.
  hdiMat <- matrix(0, nrow = length(zvec), ncol = 2)
  for (zIdx in 1:length(zvec)) {
    z <- zvec[zIdx]
    hdiMat[zIdx,] <- HDIofICDF(stats::qbeta,
                               shape1 = z + audPriorA,
                               shape2 = sampleSize - z + audPriorB)
  }
  hdiWid <- hdiMat[, 2] - hdiMat[, 1]
  hdiWid_expected[length(hdiWid_expected) + 1] <- sum(hdiWid * pzvec)

  # Store all the possible HDI widths
  # hdiwid_l[[length(hdiwid_l) + 1]] <- tibble(pzvec, hdiWid, n)


  return(data.frame(sampleSize,
                    HDIwidth = hdiWid_expected))

}
