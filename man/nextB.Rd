\name{nextB}
\alias{nextB}
\title{
Selects the next set of individual coefficients.
}
\description{
 This uses a Metropolis-Hastings algorithm for generating the next set of individual-level coefficients. 
}
\usage{
nextB(a, b, d, p, f)
}
\arguments{
   \item{a}{ The current means of the underlying normal distributions for the set of random parameters - a vector of length gNIV }
   \item{b}{ The current values for the normally distributed individual-level parameters - a gNP x gNIV matrix }	
   \item{d}{ The current variance-covariance matrix for the set of random parameters - a gNIV x gNIV matrix }
   \item{p}{ The current likelihood values for each individual - a vector  of length gNP }
   \item{f}{ The current value of the fixed (non-random) parameters - a vector of length gFIV }
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\note{

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
