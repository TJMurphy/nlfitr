#' Fit dose-response when dose is linear scale.
#'
#' fitlindr uses the nlsLM function from the minpack.lm package to fit a
#' model formula to nonlinear response data. The formula is based upon the
#' general hyperbolic function: y/ymax=x^h/(x^h+k^h), where ymax = yhi - ylo.
#' k, ylo, yhi and h are start list parameters for nlsLM.
#' The regression formula is `y ~ ylo + (yhi - ylo)*x^h/(x^h + k^h)`
#'
#'
#' @param x a vector of linear scale values, usually dose or concentration.
#' @param y a vector of y values for x, usually responses.
#' @param data a dataframe with x and y.
#' @param k estimates the value of x that yields y/ymax = 0.5, usually ED50 or EC50.
#' @param ylo estimates the lowest asymptotic y value, in response units.
#' @param yhi estimates highest asymptotic value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param weigh chooses regression weighting by 1/y^2. Default = FALSE.
#'
#' @return nls
#' @export
#'
#' @examples
#' ## aorta contraction by phenylephrine doses
#' fitlindr(x=nM_PE, y=mN, data=aoc, k=100, ylo=0, yhi=50, h=1, weigh=TRUE)
#'
fitlindr <- function(x, y, data, k, ylo, yhi, h, weigh){

  x <- substitute(x)

  formula <- paste(substitute(y), "~ ylo+(yhi-ylo)*", x, "^h/(", x,"^h+k^h)", sep="")

  y <- eval(substitute(y), data)
  weight <- 1/y^2

  data <- dplyr::bind_cols(data, weight=weight)

  if (weigh) {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k, ylo=ylo, yhi=yhi, h=h),
                               data=data,
                               weights = weight)
    model
  }

  else {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k, ylo=ylo, yhi=yhi, h=h),
                               data=data)
    model
  }
}
