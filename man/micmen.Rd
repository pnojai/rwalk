% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rwalk.R
\name{micmen}
\alias{micmen}
\title{Michaelis-Menten}
\usage{
micmen(x, vmax, km, duration)
}
\arguments{
\item{x}{Concentration in micromoles}

\item{vmax}{Micromoles}

\item{km}{Micromoles}

\item{duration}{Seconds}
}
\value{
numeric
}
\description{
Correct for uptake according to the Michaelis-Menten equation.
}
\examples{
micmen(x = 1.375, vmax = 4.57, km = .78, duration = .007407)
}
