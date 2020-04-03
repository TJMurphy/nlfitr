#' Simulate heteroscedastic exponential growth data
#'
#' A sandbox to simulate heteroscedastic exponential growth data.
#' Random normal variance enlarges with the model value of y using
#' a constant coefficeint of variation (cv). The data generating
#' model is an exponential growth function: `yp=ylo*exp(k*x)`.
#' The simulated value is then `y=rnorm(1, yp, cv*yp)`.
#' The plot fitting subfunction will occasionally fail due to random
#' data. These may occur with higher cv values, fewer reps, and
#' fewer x values. Just re-simulate with modified parameter values.
#'
#' @param x A vector of linear scale values.
#' @param k A starting estimate for the rate constant, which has
#' units of reciprocal x.
#' @param ylo The lowest value of y, or starting value.
#' @param cv The coefficient of variation.
#' @param reps The number of replicates per value of x.
#' @param weight A logical value indicating y scale weighting. Default is FALSE.
#' If TRUE, the curve is fit using relative (1/y^2) weighting.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # Hypothetical Cell growth measured every 4 hours, replicated 5 times
#'
#' simu <- simhetegro(x=c(0, 4, 8, 12, 16, 20, 24, 28), k=0.2, ylo=100,
#' cv=0.3, reps=5, weight=FALSE)
#'
#' simu
#'
#' simu$data#'
#'
simhetegro <- function (x, k, ylo, cv, reps, weight=F) {

  yp <- ylo*exp(k*x)

  values <- data.frame(x=rep(x, reps), yp)

  y <- c()

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="model: y=ylo*exp(k*x)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      ggplot2::aes(weight = if (weight){
        weight = 1/y^2} else {weight = 1} ),
      formula = "y ~ylo*exp(k*x)",
      method.args = list(
        start=c(ylo=ylo,
                k=k)
      ),
      se=F,
      color="blue"
    )
}
