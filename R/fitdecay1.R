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
#' x <- decay1example#Time
#' y <- decay1example$Response
#' # fitdecay1(x=x y=y, data=decay1example, k=0.2, ylo=1, yhi=100, weigh=T)
#'
fitdecay1 <- function(x, y, data, k, ylo, yhi, weigh){
  

  x <- substitute(x)

  formula <- paste(substitute(y), "~ (yhi-ylo)*exp(-1*k*", x), "+ylo", sep="")

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

#time <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45)
#decay1dat <- simdecay1(time, k = 0.15, ylo = 1, yhi = 100, cv = 0.20, reps = 5)
#fitdecay1(x=x, y=y, data=decay1dat$data, k=0.2, ylo=1, yhi=100, weigh=T)

#a = decay1dat$data
#a = a[,c(1,3)]
#colnames(a) = c("Time", "Response")
#a = a[order(a$Time),]
#a = data.frame(a, Replicate = rep(paste("V",1:5,sep=""),10))
x = a$Time
y = a$Response
fitdecay1(x=x, y=y, data=a, k=0.2, ylo=1, yhi=100, weigh=T)
