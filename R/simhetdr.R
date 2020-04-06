#' Simulate nonlinear heteroscedastic dose-response data
#'
#' A sandbox to simulate and visualize random normal heteroscedastic response data.
#' Variances enlarge with the value of y predicted by the model using a constant
#' coefficeint of variation (cv). The data generating formula is derived from the
#' general hyperbolic model: y/ymax=x^h/(x^h+k^h). Failure errors in the plot
#' fitting subfunction will occasionally happen due to the random data. These are
#' more frequent with higher cv values. Just re-simulate with modified parameter values.
#' The regression formula is `y ~ ylo + (yhi - ylo)*x^h/(x^h + k^h)`
#'
#' @param x a vector of non-exponential linear scale values, usually representing dose or concentration, but can represent any stimulus.
#' @param k the value of x that yields y/ymax = 0.5, usually EC50 or ED50.
#' @param ylo the lowest expected y value, in response units.
#' @param yhi the highest expected y value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param cv the coefficient of variation for y replicates.
#' @param reps an integer value for number of replicates
#' @param weight logical value indicating y scale weighting. Default is FALSE. If TRUE, curve is fit using relative (1/y^2) weighting.
#' @param log logical value. Default is FALSE. If TRUE, linear x values are transformed using a log10 function for plotting. Only for visual aesthetic.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # Note: exponential or log-transformed x scale values will not work
#' # do not use x = c(1e-9, 3e-9, ...) or c(-9, -8.523, ...)
#'
#' dose <- c(1, 3, 10, 30, 100, 300) # eg, in nM units
#'
#' set.seed(2345)
#'
#' hetdat <- simhetdr(dose, k = 35, ylo = 100, yhi = 1000,
#' h = 1.0, cv = 0.10, reps = 5, weight=TRUE, log = TRUE ); hetdat
#'
#' hetdat$data
#'
#'
simhetdr <- function (x, k, ylo, yhi, h, cv, reps, weight=F, log=F) {

  yp <- ylo + (yhi - ylo)*x^h/(x^h + k^h)

  values <- data.frame(x=rep(x, reps), yp)

  y <- c()

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x=if (log){
      log10(x)} else {
        x}, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="model: y=ymax*x/(x+k)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
      ggplot2::aes(weight = if (weight){
        weight = 1/y^2} else {weight = 1} ),
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
