simlogdr <- function(x, logk, ylo, yhi, h, sd, reps) {

  values <- data.frame(x=rep(x,reps))
  values <- mutate(values, y= ylo+(yhi-ylo)/(1+10^((logk-x)*h)) + rnorm(length(x), 0, sd))

  ggplot(
    values,
    aes(x, y))+
    geom_point(size=1)+
    labs(title="y=ylo+(yhi-ylo)/(1+10^((logk-x)*h)) + rnorm(length(x), 0, sd)")
}
