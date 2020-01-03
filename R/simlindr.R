#' Simulate dose-response
#'
#' Creates and ggplots replicate dose-response data with random normal error based on a linear scaled predictor variable and additional parameter arguments. Derived from the general hyperbolic function: y/ymax=logx^h/(logx^h+logk^h), where ymax = yhi - ylo.
#'
#' @param x A vector of linear scale values, usually dose or concentration..
#' @param k The value of x that yields y/ymax = 0.5, usually EC50 or ED50
#' @param ylo The lowest expected y value, in response units.
#' @param yhi The highest expected y value, in response units.
#' @param h The Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param sd Standard deviation of residual error, in response units.
#' @param reps Integer value for number of replicates.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' Up <- simlindr(x = c(1, 3, 30, 100, 300), k = 30,
#' ylo = 300, yhi = 3000,
#' h = 1.0,
#' sd = 100, reps = 5); Up
#'
#' Up$data
#'
#' Down <- simlindr(x = c(1, 3, 30, 100, 300), k = 30,
#' ylo = 300, yhi = 3000,
#' h = -1.0,
#' sd = 100, reps = 5); Down
#'
#'
#'
simlindr <- function(x, k, ylo, yhi, h, sd, reps) {

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + rnorm(length(x), 0, sd)") +
    ggplot2::geom_smooth(
      method=stats::nls,
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
