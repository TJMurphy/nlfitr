#' Fit dose-response when dose is log10 scale
#'
#' fitlogdr uses the nlsLM function of the minpack.lm package to fit a model
#' formula to nonlinear response data. The formula is based upon the general
#' hyperbolic function y= ymax/(1+10^((logk-x)*h)), where ymax = yhi - ylo.
#' Both x and logk are in base 10 log scale units. logk, ylo, yhi and h are
#' start list parameters #' for nlsLM. The formula is: `y = ylo + (yhi - ylo)/(1 + 10^((logk - x)*h))`
#'
#'
#' @param x a vector of base 10 log scale values, usually dose or concentration.
#' @param y a vector of y values for x, usually responses.
#' @param data a dataframe with x and y.
#' @param logk estimates the value of base 10 log scale x that yields y/ymax = 0.5, usually ED50 or EC50.
#' @param ylo estimates the lowest asymptotic y value, in response units.
#' @param yhi estimates highest asymptotic value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param weigh chooses regression weighting by 1/y^2. Default = FALSE.
#'
#' @return nls
#' @export
#'
#' @examples
#' # aorta contraction by phenylephrine doses
#' fitlogdr(x=logM_PE, y=mN, data=aoc, logk=-8, ylo=0, yhi=50, h=1, weigh=TRUE)
#'
#' # or
#' fitlogdr(x=lognM_PE, y=mN, data=aoc, logk=1, ylo=0, yhi=50, h=1, weigh=TRUE)
#'
#'
#'
fitlogdr <- function(x, y, data, logk, ylo, yhi, h, weigh){

  x <- substitute(x)

  formula <- paste(substitute(y), " ~ ylo + ((yhi - ylo)/(1 + 10^((", "logk -", x, ")*h)))", sep="")

  y <- eval(substitute(y), data)
  weight <- 1/y^2

  data <- dplyr::bind_cols(data, weight=weight)

  if (weigh) {

    model <- minpack.lm::nlsLM(formula,
                               start=list(logk=logk, ylo=ylo, yhi=yhi, h=h),
                               data=data,
                               weights = weight)
    model
  }

  else {

    model <- minpack.lm::nlsLM(formula,
                               start=list(logk=logk, ylo=ylo, yhi=yhi, h=h),
                               data=data)
    model
  }
}
