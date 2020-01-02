#' Simulate log dose-response
#'
#' Creates and ggplots simulated replicate dose-response data with random replicate error based on a log scaled predictor variable and additional parameter arguments. Derived from the general hyperbolic function: y/ymax=logx^h/(logx^h+logk^h), where ymax = yhi - ylo.
#'
#' @param x A vector of log scale values; usually log10 or log2 dose or concentration units.
#' @param logk The value of x in log units that yields y/ymax = 0.5; usually ED50 or EC50.
#' @param ylo The minimum expected y value, in response units.
#' @param yhi The highest expected y value, in response units.
#' @param h The Hill slope; h > 1 is steeper, h < 1 is shallower.
#' @param sd Standard deviation of residual error.
#' @param reps Integer value for number of replicate values.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' sim <- simlogdr(x = c(-10, -9, -8, -7, -6),
#'                logk = -8, ylo = 300, yhi = 3000,
#'                h = 1.8, sd = 100, reps = 3); sim
#' sim$data
#'
#'
simlogdr <- function(x, logk, ylo, yhi, h, sd, reps) {

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)/(1+10^((logk-x)*h)) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y))+
    ggplot2::geom_point(size=1)+
    ggplot2::labs(title="y = ylo + (yhi - ylo)/(1 + 10^((logk - x)*h)) + rnorm(length(x), 0, sd)")
}
