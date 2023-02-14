#' Add a shaded area under the beta density curve
#' Assumes inherited data with columns p and density
#' @param pmin Minimum value of p to shade.
#' @param pmax Maximum value of p to shade.
#' @export
geom_betaArea <- function (pmin, pmax) {
  ggplot2::geom_area(
    data = function (x) {dplyr::filter(x, x$p >= pmin, x$p <= pmax)},
    fill = "darkgrey"
  )
}
