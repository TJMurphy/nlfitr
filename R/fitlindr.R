fitlindr <- function(x, y, data, k, ylo, yhi, h, weigh){

  x <- substitute(x)

  formula <- paste(y, "~ ylo+(yhi-ylo)*", x, "^h/(", x,"^h+k^h)", sep="")

  y <- eval(substitute(y), data)
  weight <- 1/y^2

  data <- bind_cols(data, weight=weight)

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
