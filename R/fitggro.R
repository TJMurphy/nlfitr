#' Fit Gompertz model for growth curves
#'
#' Fit the Gompertz growth model to date:
#' `y =  ylo + d*exp(-exp(((k*exp(1))/d)*(lambda-x)+1))`
#'
#'
#' @param x A vector of linear values, usually time.
#' @param y A vector of measured values in response to x.
#' @param data A dataframe with x and y.
#' @param k The estimate for maximal growth rate.
#' @param ylo The estimate for the lowest value of y.
#' @param d The estimate for the difference between the
#' lowest and highest values of y.
#' @param lambda The estimate for the growth lag, in units of the x scale.
#' @param weigh When TRUE, weighted regression by 1/y^2. Default is FALSE.
#'
#' @return nls
#' @export
#'
#' @examples
#'
#' # L.plantarium growth at 35C.
#'
#' fitggro(x=hour, y=lnN_N0, data=plantdat,
#' k=1, ylo=0, d=8, lambda=3, weigh=FALSE)
#'
#'
fitggro <- function(x, y, data, k, ylo, d, lambda, weigh){

  x <- substitute(x)

  formula <- paste(substitute(y),
                   "~ylo+d*exp(-exp(((k*exp(1))/d)*(lambda-", x,")+1))",
                   sep="")

  y <- eval(substitute(y), data)

  weight <- 1/y^2

  data <- dplyr::bind_cols(data, weight=weight)

  if (weigh) {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k,
                                          ylo=ylo,
                                          d=d,
                                          lambda=lambda),
                               data=data,
                               weights = weight)
    model
  }

  else {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k,
                                          ylo=ylo,
                                          d=d,
                                          lambda=lambda),
                               data=data)
    model
  }
}
