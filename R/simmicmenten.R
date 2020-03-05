#' Simulate Michaelis-Menten substrate vs. enzyme velocity data
#'
#'A sandbox to simulate and visualize random normal heteroscedastic response data. Variances enlarge with the value of y predicted by the model using a constant coefficeint of variation (cv). The data generating formula is derived from the Michaelis-Menten model equation including considerations for allosteric modulation: y=vmax*x^h/(x+km^h). Failure errors in the plot fitting subfunction will occasionally happen due to the random data. These are more frequent with higher cv values. Just re-simulate with modified parameter values. Additionally, loss of data points may occur via geom_smooth(), or nlsLM will give a warning if x values approach 0 when log = TRUE and your hill slope (h) is != 1. If problems occur, and you want to simulate high h values, consider linearizing your input vector x and rescaling so that integer values can be used that represent low values (like nM).
#'
#' @param x a vector representing dose or concentration
#' @param vmax the maximum enzyme velocity possible. The curve approaches this value with increasing concentrations of substrate.
#' @param km the value of x that yields y/vmax = 0.5 (the Michaelis-Menten constant)
#' @param cv the coefficient of variation for y replicates
#' @param reps an integer value for number of replicates
#' @param h a value representing the hill slope coefficient. The default is h = 1 which considers enzyme-substrate interactions occuring only at 1 site on the enzyme. When h != 1, it indicates cooperativity (positive if h > 1; negative if h < 1). The h coefficient will not always equal the number of true binding sites, but it cannot exceed the number of true binding sites.
#' @param log logical value. Default is FALSE. If TRUE, linear x values are transformed using a log10 function for plotting. Only for visual aesthetic.
#'
#' @return ggplot, data
#' @export
#'
#' @examples
#'
#' # Note: Log-transformed x scale values will not work
#' # do not use c(-9, -8.523, ...)
#'
#' dose <- c(1, 3, 10, 30, 100, 300) # eg, in nM units
#' logdose <- c(1e-3, 3e-3, 1e-2, 3e-2, 1e-1, 3e-1, 1, 3, 1e1, 3e1,
#'              1e2, 3e2, 1e3, 3e3, 1e4, 3e4, 1e5, 3e5) # eg, in M units
#'
#' set.seed(2345)
#'
#' vdat <- simmicmenten(dose, vmax = 1000, km = 50, cv = 0.10, reps = 5, h = 1, log = FALSE ); vdat
#'
#' vdat$data
#'
#' # Note: Exponential values (i.e. 1e-3, etc.) will produce errors for h !=1. Use integers if you want log = T and h != 1
#'
#' simmicmenten(dose, vmax = 10000, km = 100, cv = 0.25, reps = 10, h = 2, log = TRUE)
#' simmicmenten(logdose, vmax = 1000, km = 2, cv = 0.1, reps = 5, h = 1, log = TRUE)
#'
#'
simmicmenten <- function(x, vmax, km, cv, reps, h = 1, log=F) {

  y = vmax*x^h/(km^h + x)

  values <- data.frame(x=rep(x, reps), y)

  values <- dplyr::mutate(values, y=apply(values, 1, function(x) stats::rnorm(1, x[2], cv*x[2])))

  for (i in 1) {
    if (log) {
      model <- stats::lm(log(y) ~ log(x), data=values)
      start <- list(vmax=exp(stats::coef(model)[1]), km=exp(stats::coef(model))[2])
      model <- stats::nls(y ~ vmax * x^h/(km^h + x), data = values, start = start)
      vmax.var <- model$m$getPars()[1]
      km.var <- model$m$getPars()[2]
      break
    } else {break}
  }

  logplot = function() {list(
            ggplot2::stat_function(geom = "smooth", fun = function(x) y = vmax.var*10^x^h/(km.var^h + 10^x), color = "blue"),
            ggplot2::labs(title="model: y=vmax*log10x^h/(log10x+log10km^h)"),
            ggplot2::xlab("log10 x")
            )
    }

  linplot = function() {list(
            ggplot2::geom_smooth(method=minpack.lm::nlsLM, formula = "y ~vmax*x^h/(x+km^h)", method.args = list(start= c(vmax = vmax, km = km, h = h)), se=F,color="blue"),
            ggplot2::labs(title="model: y=vmax*x^h/(x+km^h)"),
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

