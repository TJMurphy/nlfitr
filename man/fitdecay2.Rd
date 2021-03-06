% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitdecay2.R
\name{fitdecay2}
\alias{fitdecay2}
\title{Fit two phase exponential decay data.}
\usage{
fitdecay2(x, y, data, k1, k2, range1, range2, ylo, weigh)
}
\arguments{
\item{x}{is a vector of non-exponential linear scale values representing time.}

\item{y}{is a vector of y values for x, usually responses.}

\item{data}{is a dataframe with x and y.}

\item{k1}{the first rate constant, expressed in reciprocal of the X axis time units.
The first half-life is 0.6932/k1.}

\item{k2}{the second rate constant, expressed in reciprocal of the X axis time units.
The second half-life is 0.6932/k2.}

\item{range1}{a single value for the range of y in the first phase of decay, in y units.}

\item{range2}{a single value for the range of y in the second phase of decay, in y units.}

\item{ylo}{the lowest y value, or the value at infinite times,
expressed in the same units as Y.}

\item{weigh}{chooses regression weighting by 1/y^2. Default = FALSE.}
}
\value{
nls
}
\description{
Based on a two phase exponential decay model `y=range1*exp(-k1*x)+range2*exp(-k2*x)`
The fitting formula is: `y ~range1*exp(-k1*x) + range2*exp(-k2*x) + ylo`
}
\examples{
# fitdecay2(x=Time, y=Response, data=decay2ex, k1 = 1.5, k2 = 0.2,
# range1 = 2, range2 = 10, ylo=1, weigh=T)

}
