#' Contraction of mouse aorta in response to phenylephrine
#' @source Murphy lab
#' @format A dataframe with 5 independent replicates:
#'  \describe{
#'  \item{nM_PE}{Doses of phenylephrine applied, in nM units.}
#'  \item{replicate}{name of independent replicate.}
#'  \item{mN}{measured contractile tension response in milliNewtons per mg dry weight.}
#'  \item{M_PE}{Doses of phenylephrine applied, in M units.}
#'  \item{logM_PE}{Doses of phenylephrine applied, in log10 M units.}
#'  \item{lognM_PE}{Doses of phenylephrine applied, in log10 nM units.}
#' }
#' @examples
#' \dontrun{
#'  aoc
#' }
"aoc"



#' One phase decay example data
#' @source simulated with simdecay1
#' @format A dataframe with 5 independent replicates:
#'  \describe{
#'  \item{Time}{Units of time, x.}
#'  \item{Response}{Response units, y.}
#'  \item{Replicate}{name of independent replicate.}
#' }
#' @examples
#' \dontrun{
#'  decay1ex
#' }
"decay1ex"



#' Two phase decay example data
#' @source simulated with simdecay2
#' @format A dataframe with 5 independent replicates:
#'  \describe{
#'  \item{Time}{Units of time, x.}
#'  \item{Response}{Response units, y.}
#'  \item{Replicate}{name of independent replicate.}
#' }
#' @examples
#' \dontrun{
#'  decay2ex
#' }
"decay2ex"


#' Exponential growth of bacteria
#' @source Murphy lab
#' @format A dataframe with 3 A600 measurements per time point:
#'  \describe{
#'  \item{min}{Units of time, minutes}
#'  \item{A600}{optical density at A600 nm}
#' }
#' @examples
#' \dontrun{
#'  culture
#' }
"culture"


#' Gompertz model for growth curves
#' @source simulated with simggro
#' @format A dataframe:
#'  \describe{
#'  \item{hour}{Units of time, hour}
#'  \item{lnN_N0}{l.plantarium growth measure}
#' }
#' @examples
#' \dontrun{
#'  plantdat
#' }
"plantdat"

