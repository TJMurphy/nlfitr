simlindr <- function(x, k, ylo, yhi, h, sd, reps) {

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + stats::rnorm(length(x), 0, sd))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + rnorm(length(x), 0, sd)") +
    ggplot2::geom_smooth(
      method=stats::nls,
      formula = "y ~ylo+(yhi-ylo)*x^h/(x^h+k^h)",
      method.args = list(
        start=c(yhi=yhi,
                ylo=ylo,
                k=k,
                h=h)
      ),
      se=F,
      color="blue"
    )
}
