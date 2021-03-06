% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitdecay1.R
\name{fitdecay1}
\alias{fitdecay1}
\title{Fit one phase exponential decay data.}
\usage{
fitdecay1(x, y, data, k, ylo, yhi, weigh)
}
\arguments{
\item{x}{is a vector of non-exponential linear scale values, usually representing time.}

\item{y}{is a vector of y values for x, usually responses.}

\item{data}{is a dataframe with x and y.}

\item{k}{the  rate constant, expressed in reciprocal of the X axis units.
When time, the half-life is 0.6932/k.}

\item{ylo}{the lowest y value, or the value at infinite times,
expressed in the same units as Y.}

\item{yhi}{the highest y value, or the starting value,
expressed in the same units as Y.}

\item{weigh}{chooses regression weighting by 1/y^2. Default = FALSE.}
}
\value{
nls
}
\description{
Based on the one phase exponential decay model `y=range*exp(-k*x)` where range = yhi-ylo.
The regression formula is: `y ~ (yhi-ylo)*exp(-1*k*x) + ylo``
}
\examples{
# fitdecay1(x=Time, y=Response, data=decay1ex, k=0.2, ylo=1, yhi=100, weigh=T)

}
