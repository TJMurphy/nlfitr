#' Simulate log dose-response data
#'
#' A sandbox to create and plot replicate response data with random normal error on a log scale. Enter a log scaled predictor variable and values for additional model parameter arguments. The nonlinear model formula is derived from the general hyperbolic stimulus-response function: y/ymax=x^h/(x^h+k^h), where ymax = yhi - ylo.
#'
#' @param x a vector of log scale values; usually log10 or log2 dose or concentration units.
#' @param logk the value of x in log units that yields y/ymax = 0.5; usually logEC50 or logED50.
#' @param ylo the lowest expected y value, in response units.
#' @param yhi the highest expected y value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Provide negative value for downward sloping response.
#' @param sd the standard deviation of residual error, in response units.
#' @param reps and integer value for number of replicates.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # x is equivalent to log10(c(1e-10, 3e-10, 1e-9, 3e-9, 1e-8, 3e-8, 1e-7, 3e-7, 1e-6)).
#' # note these yield approximately half unit spacing on log10 scale
#'
#' simUp <- simlogdr(x = c(-10, -9.523, -9, -8.523, -8, -7.523, -7, -6.523, -6),
#'                logk = -8, ylo = 300, yhi = 3000,
#'                h = 1.0, sd = 100, reps = 5); simUp
#'
#' # grab simulated data for other uses
#'
#' simUp$data
#'
#' # use negative values of h to simulate downward sloping responses
#'
#' simDown <- simlogdr(x = c(-10, -9.523, -9, -8.523, -8, -7.523, -7, -6.523, -6),
#'                logk = -8, ylo = 300, yhi = 3000,
#'                h = -2.5, sd = 100, reps = 5); simDown
#'
#'
simlogdr <- function(x, logk, ylo, yhi, h, sd, reps) {

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)/(1+10^((logk-x)*h)) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo + (yhi - ylo)/(1 + 10^((logk - x)*h)) + rnorm(length(x), 0, sd)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      formula = "y ~ ylo + ((yhi - ylo)/(1 + 10^((logk - x)*h)))",
      method.args = list(
        start=c(yhi=yhi,
                ylo=ylo,
                logk=logk,
                h=h)
      ),
      se=F,
      color="blue"
    )
}
