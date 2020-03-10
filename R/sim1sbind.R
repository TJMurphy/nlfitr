#' Simulate one-site ligand receptor binding
#'
#' A sandbox to simulate and visualize random normal heteroscedastic response data. Variances enlarge with the value of y predicted by the model using a constant coefficeint of variation (cv). The data generating formula is derived from the one-site total binding model: y=Bmax*x/(x+kd). Failure errors in the plot fitting subfunction will occasionally happen due to the random data. These are more frequent with higher cv values. Just re-simulate with modified parameter values.
#'
#' @param x a vector representing dose or concentration of ligand (exponential or linear values)
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
#' dose <- c(1, 3, 10, 30, 100, 300) # eg, in nM units
#' logdose <- c(1e-3, 3e-3, 1e-2, 3e-2, 1e-1, 3e-1, 1e0, 3e0) # eg, in nM units
#'
#' set.seed(2345)
#'
#' binddat <- sim1sbind(dose, bmax = 1000, kd = 50, cv = 0.10, reps = 5, log = FALSE ); binddat
#'
#' binddat$data #extract the data frame containing x and cv-modified y values
#'
#' sim1sbind(logdose, bmax = 10000, kd = 5e-2, cv = 0.20, reps = 5, log = TRUE)
#'
#'
sim1sbind <- function(x, bmax, kd, cv, reps, log=F) {

  y = bmax*x/(kd + x)

  values <- data.frame(x=rep(x, reps), y)

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  for (i in 1) {
    if (log) {
      model <- stats::lm(log10(y) ~ log10(x), data=values)
      start <- list(bmax=10^(stats::coef(model)[1]), kd=10^(stats::coef(model))[2])
      model <- minpack.lm::nlsLM(y ~ bmax * x/(kd + x), data = values, start = start)
      bmax.var <- model$m$getPars()[1]
      kd.var <- model$m$getPars()[2]
      break
    } else {break}
  }

  logplot = function() {list(
            ggplot2::stat_function(geom = "smooth", fun = function(x) y = bmax.var*10^x/(kd.var + 10^x), color = "blue"),
            ggplot2::labs(title="model: y=bmax*log10x/(log10x+log10kd)"),
            ggplot2::xlab("log10 x")
            )
    }

  linplot = function() {list(
            ggplot2::geom_smooth(method=minpack.lm::nlsLM, formula = "y ~bmax*x/(x+kd)", method.args = list(start= c(bmax = bmax,kd = kd)), se=F,color="blue"),
            ggplot2::labs(title="model: y=bmax*x/(x+kd)"),
            ggplot2::xlab("x")
            )
    }

  ggplot2::ggplot(
    values,
    ggplot2::aes(x=if (log){
      log10(x)} else {
        x}, y)) +
    ggplot2::geom_point(size=2) +
    if (log) {
      logplot()
     } else {
      linplot()
     }
}

