#' Fit one phase exponential decay data.
#' Based on a model
#'
#' @param x is a vector of non-exponential linear scale values representing time.
#' @param y is a vector of y values for x, usually responses.
#' @param data is a dataframe with x and y.
#' @param k the  rate constant, expressed in reciprocal of the X axis time units.
#' The half-life is 0.6932/k.
#' @param ylo the lowest y value, or the value at infinite times,
#' expressed in the same units as Y.
#' @param yhi the highest y value, or the starting value,
#' expressed in the same units as Y.
#' @param weigh chooses regression weighting by 1/y^2. Default = FALSE.
#'
#' @return model
#' @export
#'
#' @examples
#' # fitdecay1(x=Time y=Response, data=decay1ex, k=0.2, ylo=1, yhi=100, weigh=T)
#'
fitdecay1 <- function(x, y, data, k, ylo, yhi, weigh){


  x <- substitute(x)

  formula <- paste(substitute(y), "~ (yhi-ylo)*exp(-1*k*", x, ")+ylo", sep="")

  y <- eval(substitute(y), data)

  weight <- 1/y^2

  data <- dplyr::bind_cols(data, weight=weight)

  if (weigh) {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k, ylo=ylo, yhi=yhi),
                               data=data,
                               weights = weight)
    model
  }

  else {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k, ylo=ylo, yhi=yhi),
                               data=data)
    model
  }
}
