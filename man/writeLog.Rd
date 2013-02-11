\name{writeLog}
\alias{writeLog}
\title{
 Writes log
}
\description{
 Generates a log file as the estimation occurs. Also generates output to the screen.
}
\usage{
writeLog(r, p, a, b, d, f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{ The current iteration of the Markov chain }
  \item{p}{ The current likelihood values for each individual - a vector  of length gNP }
  \item{a}{ The current means of the underlying normal distributions for the set of random parameters - a vector of length gNIV }
  \item{b}{ The current values for the normally distributed individual-level parameters - a gNP x gNIV matrix }	
  \item{d}{ The current variance-covariance matrix for the set of random parameters - a gNIV x gNIV matrix }
  \item{f}{ The current value of the fixed (non-random) parameters - a vector of length gFIV }
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
