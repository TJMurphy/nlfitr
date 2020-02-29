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

  ggplot2::ggplot(
    values,
    ggplot2::aes(x=if (log){
      log10(x)} else {
        x}, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="model: y=vmax*x^h/(x+km^h)") +
    if (log) {
      ggplot2::stat_function(geom = "smooth", fun = function(x) y = vmax.var*10^x^h/(km.var^h + 10^x),
                             color = "blue")
    } else {
      ggplot2::geom_smooth(
        method=minpack.lm::nlsLM,
        formula = "y ~vmax*x^h/(x+km^h)",
        method.args = list(
          start= c(vmax = vmax,
                   km = km,
                   h = h)
        ),
        se=F,
        color="blue")
    }
}
