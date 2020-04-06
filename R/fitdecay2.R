#' Fit two phase exponential decay data.
#'
#' Based on a two phase exponential decay model `y=range1*exp(-k1*x)+range2*exp(-k2*x)`
#' The fitting formula is: `y ~range1*exp(-k1*x) + range2*exp(-k2*x) + ylo`
#'
#' @param x is a vector of non-exponential linear scale values representing time.
#' @param y is a vector of y values for x, usually responses.
#' @param data is a dataframe with x and y.
#' @param k1 the first rate constant, expressed in reciprocal of the X axis time units.
#' The first half-life is 0.6932/k1.
#' @param k2 the second rate constant, expressed in reciprocal of the X axis time units.
#' The second half-life is 0.6932/k2.
#' @param ylo the lowest y value, or the value at infinite times,
#' expressed in the same units as Y.
#' @param range1 a single value for the range of y in the first phase of decay, in y units.
#' @param range2 a single value for the range of y in the second phase of decay, in y units.
#' @param weigh chooses regression weighting by 1/y^2. Default = FALSE.
#'
#' @return nls
#' @export
#'
#' @examples
#' # fitdecay2(x=Time, y=Response, data=decay2ex, k1 = 1.5, k2 = 0.2,
#' # range1 = 2, range2 = 10, ylo=1, weigh=T)
#'
fitdecay2 <- function(x, y, data, k1, k2, range1, range2, ylo, weigh){


  x <- substitute(x)

  formula <- paste(substitute(y), "~ range1*exp(-1*k1*", x, ")+range2*exp(-1*k2*", x, ") + ylo", sep="")

  y <- eval(substitute(y), data)

  weight <- 1/y^2

  data <- dplyr::bind_cols(data, weight=weight)

  if (weigh) {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k1=k1, k2=k2, range1=range1, range2=range2, ylo=ylo),
                               data=data,
                               weights = weight)
    model
  }

  else {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k1=k1, k2=k2, range1=range1, range2=range2, ylo=ylo),
                               data=data)
    model
  }
}
