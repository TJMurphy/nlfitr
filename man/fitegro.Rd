% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitegro.R
\name{fitegro}
\alias{fitegro}
\title{Fit exponential growth data}
\usage{
fitegro(x, y, data, k, ylo, weigh)
}
\arguments{
\item{x}{A vector of linear scale values, usually time.}

\item{y}{A vector of measured values in response to x.}

\item{data}{A dataframe with the x and y variables.}

\item{k}{A starting estimate for the rate constant, which has
units of reciprocal x.}

\item{ylo}{An estimate for the lowest value of y, or starting value.}

\item{weigh}{Chooses regression weighting by 1/y^2. Default = FALSE.}
}
\value{
nls
}
\description{
Fit the exponential growth model `y = ylo*exp(k*x)` to data.
}
\examples{
#bacterial growth

fitegro(x=min, y=A600, data=culture, k= 1, ylo=1, weigh=FALSE)

}
