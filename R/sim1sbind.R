#' Simulate one-site ligand receptor binding
#'
#' A sandbox to simulate and visualize random normal heteroscedastic response data. Variances enlarge with the value of y predicted by the model using a constant coefficeint of variation (cv). The data generating formula is derived from the one-site total binding model: y=Bmax*x/(x+kd). Failure errors in the plot fitting subfunction will occasionally happen due to the random data. These are more frequent with higher cv values. Just re-simulate with modified parameter values.
#'
#' @param x a vector of non-exponential linear scale values, usually representing dose or concentration of ligand
#' @param bmax the measured value where the receptor population is completely saturated by ligand (i.e. maximum binding)
#' @param kd the value of x that yields y/Bmax = 0.5 (the equilibrium binding constant)
#' @param cv the coefficient of variation for y replicates
#' @param reps an integer value for number of replicates
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
#' binddat <- sim1sbind(dose, bmax = 1000, kd = 50, cv = 0.10, reps = 5, log = FALSE ); binddat
#'
#' binddat$data
#' binddat$ggplot
#'
sim1sbind <- function(x, bmax, kd, cv, reps, log=F) {

  y = bmax*x/(kd + x)

  values <- data.frame(x=rep(x, reps), y)

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  for (i in 1) {
    if (log) {
      model <- stats::lm(log(y) ~ log(x), data=values)
      start <- list(bmax=exp(stats::coef(model)[1]), kd=exp(stats::coef(model))[2])
      model <- stats::nls(y ~ bmax * x/(kd + x), data = values, start = start)
      bmax.var <- model$m$getPars()[1]
      kd.var <- model$m$getPars()[2]
      break
    } else {break}
  }

  p = ggplot2::ggplot(
    values,
    ggplot2::aes(x=if (log){
      log10(x)} else {
        x}, y)) +
    ggplot2::geom_point(size=2) +
    if (log) {
      ggplot2::stat_function(geom = "smooth", fun = function(x) y = bmax.var*10^x/(kd.var + 10^x),
                             color = "blue")
     } else {
      ggplot2::geom_smooth(
        method=minpack.lm::nlsLM,
        formula = "y ~bmax*x/(x+kd)",
        method.args = list(
          start= c(bmax = bmax,
                   kd = kd)),
        se=F,
        color="blue")
     }
    p2 = if (log) {
      ggplot2::labs(title="model: y=bmax*log10x/(log10x+log10kd)")
    } else {
      ggplot2::labs(title="model: y=bmax*x/(x+kd)")
    }

    p3 = if (log) {
      ggplot2::xlab("log10 x")
    } else {
      ggplot2::xlab("x")
    }
    (ggplot = p + p2 + p3)
}

