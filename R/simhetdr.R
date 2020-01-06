#' Simulate nonlinear heteroscedastic dose-response data
#'
#' A sandbox to simulate random heteroscedastic response data, for variances that grow as x increases. Enter a linear scaled predictor variable and additional parameter arguments, including e for the variance term. Data generating formula is derived from the general hyperbolic function: y/ymax=x^h/(x^h+k^h), where ymax = yhi - ylo. Errors in geom_smooth fitting will occasionally happen. Just re-simulate or modify parameters.
#'
#' @param x a vector of non-exponential linear scale values, usually representing dose or concentration.
#' @param k the value of x that yields y/ymax = 0.5, usually EC50 or ED50.
#' @param ylo the lowest expected y value, in response units.
#' @param yhi the highest expected y value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param sd the standard deviation of residual error, in response units.
#' @param e a numeric value used as an exponent to simulate heteroscedastic standard deviations (sdh): sdh=sd+sqrt(x^e)
#' @param reps an integer value for number of replicates
#' @param log logical value indicating x-axis scale for plot. Default is FALSE. If TRUE, the x values are transformed using a log10 function for a log plot.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # this will work: x = c(1, 3, 10, 30, ...)
#' # this will not work: x = c(1e-9, 3e-9, 10e-9, 30e-9, ...)
#' # this will not work: x = c(-9, -8.523, -8, -7.523, ...)
#'
#' dose <- c(1, 3, 10, 30, 100, 300) # eg, in nM units
#'
#' hetdat <- simhetdr(dose, k = 35, ylo = 100, yhi = 1000,
#' h = 1.5, sd = 30, e = 1.5, reps = 5, log = TRUE ); hetdat
#'
#' hetdat$data
#'
#'
simhetdr <- function(x, k, ylo, yhi, h, sd, e, reps, log=F) {

  values <- data.frame(x=rep(x,reps), sdh=sd+sqrt(x^e))
  y <- c()
  sdh <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + stats::rnorm(length(x), 0, sdh))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x=if (log){
      log10(x)} else {
        x}, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + rnorm(length(x), 0, sdh)") +
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

