#' Simulate growth curves
#'
#' This is a sandbox to simulate a growth process, beginning with a
#' low initial lag period, through an exponential growth phase,
#' to a higher final maximal asymptope. The function is based
#' upon the Gompertz model, using the formula:
#' `y =  ylo + d*exp(-exp(((k*exp(1))/d)*(lambda-x)+1)) + rnorm(length(x), 0, sd)`.
#'
#' @param x A vector of linear scale values, such as time.
#' @param k The maximal growth rate, approximately the slope of the l
#' ine in the growth phase.
#' @param ylo The lowest value of y, or a starting value.
#' @param d The difference between ylo and the highest value of y.
#' @param lambda The lag period, in units of the x scale
#' @param sd The standard deviation of the replicates.
#' @param reps The number of replicates per value of x.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # simulates l.plantarium growth, see Fig 3 in Applied Environ Micro 56:1875, 1990
#'
#' plantarium <- simggro(x=c(1:8, 12, 13, 24),
#'   k=2, ylo=0, d=9, lambda=2.5, sd=0.5, reps=1)
#'
#'plantarium
#'
#'plantarium$data
#'
#'
simggro <- function(x, k, ylo, d, lambda, sd, reps){

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+d*exp(-exp(((k*exp(1))/d)*(lambda-x)+1)) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y =  ylo+d*exp(-exp(((k*exp(1))/d)*(lambda-x)+1)) + rnorm(length(x), 0, sd)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      formula = "y ~  ylo+d*exp(-exp(((k*exp(1))/d)*(lambda-x)+1))",
      method.args = list(
        start=c(ylo=ylo,
                d=d,
                lambda=lambda,
                k=k)
      ),
      se=F,
      color="blue"
    )
}
