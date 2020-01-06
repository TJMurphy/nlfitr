#' Simulate nonlinear heteroscedastic dose-response data
#'
#' A sandbox to simulate data
#'
#' @param x
#' @param k
#' @param ylo
#' @param yhi
#' @param h
#' @param sd
#' @param ex
#' @param reps
#' @param log
#'
#' @return
#' @export
#'
#' @examples
simhetdr <- function(x, k, ylo, yhi, h, sd, ex, reps, log=F) {

  values <- data.frame(x=rep(x,reps))
  y <- c()
  values <- dplyr::mutate(values, y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + stats::rnorm(length(x), 0, sd+sqrt(x^ex)))

  ggplot2::ggplot(
    values,
    ggplot2::aes(x=if (log){
      log10(x)} else {
        x}, y)) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(title="y = ylo+(yhi-ylo)*x^h/(x^h+k^h) + rnorm(length(x), 0, sdh)") +
    ggplot2::geom_smooth(
      method=minpack.lm::nlsLM,
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
