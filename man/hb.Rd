\name{hb}
\alias{hb}
\title{
 Controls the hierarchical Bayesian estimation
}
\description{
 This function controls the Gibbs sampler for the model estimation. This function is called directly from go() and does not need to be called by the user.
}
\usage{
hb(a, b, d, f)
}
\arguments{
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
