#' Simulate exponential growth
#'
#' A sandbox to simulate exponential growth with random normal error.
#' Enter a linear scale predictor variable, x, with arguments for rate
#' constant, k, and initial starting value of response, ylo. Include an
#' estimate for the standard deviation, sd, in y.  Values for y and
#' best fit curve are based upon an exponential growth function:
#' `y ~ ylo*exp(k*x) + rnorm(length(x), 0, sd)`.
#'
#'
#' @param x A vector of linear scale values, such as time..
#' @param k The  rate constant, expressed in reciprocal of the X axis units.
#' When time, the half-life is 0.6932/k.
#' @param ylo The lowest value of y, or a starting value.
#' @param sd The standard deviation of y.
#' @param reps An integer value for number of replicates.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#' # x is time in minutes
#'
#' simegro(x=c(1:10), k=0.3, ylo=10, sd=10, reps=3)
#'
#'
simegro <- function(x, k, ylo, sd, reps){

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo*exp(k*x) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo*exp(k*x) + rnorm(length(x), 0, sd)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      formula = "y ~ ylo*exp(k*x)",
      method.args = list(
        start=c(ylo=ylo,
                k=k)
      ),
      se=F,
      color="blue"
    )
}
