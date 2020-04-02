#' Fit exponential growth data
#'
#' Fit the exponential growth function `y = ylo*exp(k*x)` model to data.
#'
#' @param x A vector of linear scale values, usually time.
#' @param y A vector of measured values in response to x.
#' @param data A dataframe with the x and y variables.
#' @param k A starting estimate for the rate constant, which has
#' units of reciprocal x.
#' @param ylo An stimate for the lowest value of y, or starting value.
#' @param weigh Chooses regression weighting by 1/y^2. Default = FALSE.
#'
#' @return nls
#' @export
#'
#' @examples
#' #bacterial growth
#'
#' fitegro(x=min, y=A600, data=culture, k= 1, ylo=1, weigh=FALSE)
#'
fitegro <- function(x, y, data, k, ylo, weigh){


  x <- substitute(x)

  formula <- paste(substitute(y), "~ (ylo)*exp(k*", x, ")", sep="")

  y <- eval(substitute(y), data)

  weight <- 1/y^2

  data <- dplyr::bind_cols(data, weight=weight)

  if (weigh) {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k, ylo=ylo),
                               data=data,
                               weights = weight)
    model
  }

  else {

    model <- minpack.lm::nlsLM(formula,
                               start=list(k=k, ylo=ylo),
                               data=data)
    model
  }
}
