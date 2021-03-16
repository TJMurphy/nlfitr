#' Simulate two phase exponential decay data
#'
#' A sandbox to simulate and visualize random normal heteroscedastic response data.
#' Variances enlarge with the value of y predicted by the model using a constant
#' coefficeint of variation (cv). The data generating formula is derived from the
#' general model: "y = y0*e^-(k1+k2)x". A two-phase model is used when the outcome
#' you measure is the result of the sum of a fast and slow exponential decay.
#' This is also called a double exponential decay.Failure errors in the plot
#' fitting subfunction will occasionally happen due to the random data. These are
#' more frequent with higher cv values. Just re-simulate with modified parameter values.
#' The regression formula is: `y ~ range1*exp(-k1*x) + range2*exp(-k2*x) + ylo`
#'
#' @param x a vector of non-exponential linear scale values representing time.
#' @param k1 the first rate constant, expressed in reciprocal of the X axis time units.
#' The first half-life is 0.6932/k1.
#' @param k2 the second rate constant, expressed in reciprocal of the X axis time units.
#' The second half-life is 0.6932/k2.
#' @param range1 a single value for the range of y in the first phase of decay, in y units.
#' @param range2 a single value for the range of y in the second phase of decay, in y units..
#' @param ylo the lowest expected y value, or the value at infinite times, expressed
#' in the same units as Y.
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
#' decay2dat <- simhetdecay2(time, k1 = 0.5, k2 = 0.2, range1 = 2, range2 = 10,
#' ylo = 1.0, cv = 0.10, reps = 5)
#'
#' decay2dat
#'
#' decay2dat$data
#'
#'
simhetdecay2 <- function(x, k1, k2, range1, range2, ylo, cv, reps) {

  yp <- range1*exp(-k1*x) + range2*exp(-k2*x) + ylo

  values <- data.frame(x=rep(x, reps), yp)

  y <- c()

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  ggplot2::ggplot(values,
                  ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="model: y = range1*exp(-k1*x) + range2*exp(-k2*x) + ylo") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      formula = "y ~range1*exp(-k1*x) + range2*exp(-k2*x) + ylo",
      method.args = list(start=c(range1=range1, range2=range2, k1=k1, k2=k2, ylo=ylo)), se=F, color="blue")
}
