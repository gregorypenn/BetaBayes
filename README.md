# BetaBayes
A package for Bayesian analysis of probability estimates using beta distributions with R

## Overview

The purpose of this package for R is to provide functions for visualizing and estimating beta distributions, their parameters, and quantiles.
Some examples of functionality include:

* Plot a beta distribution with optional interval shading and line indicating expected value.
* Determine the bounds of a credible interval containing a specified percent of the probability.
* Estimate the parameters of a beta distribution from quantiles.
* Calculate the expectation of a beta distribution from its parameters.
* Determine the expected width of a confidence interval based on sample size.
* ggplot2 special geoms for interval shading and line indicating expected value.

## Project Status

**Early pre-release development phase:** _caveat emptor_

Updates may break backward-compatibility for a while. Suggestions and code contributions via pull requests welcome!

## Installation

The easiest way to install BetaBayes is by using the devtools package.

```
devtools::install_github("gregorypenn/BetaBayes")
```


