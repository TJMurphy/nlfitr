#' Simulate one phase exponential decay data
#'
#' A sandbox to simulate and visualize random normal heteroscedastic response data.
#'  Variances enlarge with the value of y predicted by the model using a constant
#'  coefficeint of variation (cv). The data generating formula is derived from the
#'  general model: "y = y0*e^-kx". A one-phase model is used when It is used whenever
#'  the rate at which something happens is proportional to the amount which is left.
#'   Failure errors in the plot  fitting subfunction will occasionally happen due
#'   to the random data. These are more frequent with higher cv values. J
#'   ust re-simulate with modified parameter values.
#'
#' @param x a vector of non-exponential linear scale values representing time.
#' @param k the  rate constant, expressed in reciprocal of the X axis time units.
#' The half-life is 0.6932/k.
#' @param ylo the lowest expected y value, or the value at infinite times,
#' expressed in the same units as Y.
#' @param yhi the highest expected y value, or the starting value,
#' expressed in the same units as Y.
#' @param cv the coefficient of variation for y replicates.
#' @param reps an integer value for number of replicates
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # Note: exponential or log-transformed x scale values will not work
#' # do not use x = c(1e-9, 3e-9, ...) or c(-9, -8.523, ...)
#'
#' time <- c(1, 5, 10, 15, 20, 25) # eg, in mins
#'
#' set.seed(2345)
#'
#' decay1dat <- simdecay1(time, k = 0.15, ylo = 1, yhi = 100, cv = 0.10, reps = 5)
#'
#'
#' decay1dat$data
#'
#'
simdecay1 <- function(x, k, ylo, yhi, cv, reps) {

  yp <- (yhi-ylo)*exp(-1*k*x) + ylo

  values <- data.frame(x=rep(x, reps), yp)

  y <- c()

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  ggplot2::ggplot(values,
                  ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="model: y=(yhi-ylo)*exp(-kx) + ylo") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      formula = "y ~(yhi-ylo)*exp(-1*k*x) + ylo",
      method.args = list(start=c(yhi=yhi, ylo = ylo, k=k)), se=F, color="blue")
  }

