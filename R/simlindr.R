#' Simulate dose-response data
#'
#' A sandbox to create and plot replicate dose-response data with random normal error. Enter a linear scaled predictor variable and additional parameter arguments. Derived from the general hyperbolic function: y/ymax=x^h/(x^h+k^h), where ymax = yhi - ylo. Errors in geom_smooth fitting will occasionally happen. Just re-simulate or modify parameters.
#'
#' @param x a vector of linear scale values, usually dose or concentration.
#' @param k the value of x that yields y/ymax = 0.5, usually EC50 or ED50.
#' @param ylo the lowest expected y value, in response units.
#' @param yhi the highest expected y value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param sd the standard deviation of residual error, in response units.
#' @param reps an integer value for number of replicates.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # example of x-axis units in nM
#'
#' Up <- simlindr(x = c(1, 3, 30, 100, 300), k = 30,
#' ylo = 300, yhi = 3000,
#' h = 1.0,
#' sd = 100, reps = 5); Up
#'
#' # use data for other purposes
#'
#' Up$data
#'
#' # negative h values simulate downward sloping response
#' conc <- c(1e-9, 3e-9, 1e-8, 3e-8, 1e-7, 3e-7)
#' Down <- simlindr(x = conc, k = 30,
#' ylo = 300, yhi = 3000,
#' h = -1.0,
#' sd = 100, reps = 5); Down
#'
#'
#'
simlindr <- function(x, k, ylo,  yhi, h, sd, reps) {

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + rnorm(length(x), 0, sd)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      formula = "y ~ylo+(yhi-ylo)*x^h/(x^h+k^h)",
      method.args = list(
        start=c(yhi=yhi,
                ylo=ylo,
                k=k,
                h=h)
      ),
      se=F,
      color="blue"
    )
}
