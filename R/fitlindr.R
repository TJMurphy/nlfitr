#' Fit dose response when dose is linear scale.
#' Based on a model
#'
#' @param x is a vector of linear scale values, usually dose or concentration.
#' @param y is a vector of y values for x, usually responses.
#' @param data is a dataframe with x and y.
#' @param k the value of x that yields y/ymax = 0.5, usually ED50 or EC50.
#' @param ylo is the lowest asymptotic y value, in response units.
#' @param yhi the highest asymptotic value, in response units.
#' @param h the Hill slope, a unitless slope factor; -1 > h > 1 is steeper, -1 < h < 1 is shallower. Use negative value for downward sloping response.
#' @param weigh chooses regression weighting by 1/y^2. Default = FALSE.
#'
#' @return model
#' @export
#'
#' @examples
#' # fitlinder(x=PE, y=mN, data=aoc, k=100, ylo=0, yhi=50, h=1, weigh=T)
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
