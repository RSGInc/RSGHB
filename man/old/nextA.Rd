\name{nextA}
\alias{nextA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Selects the next draw of A
}
\description{
 The A vector contains the means of the underlying normal distributions for the random coefficents. This function provides the next set of values for the A matrix.
 }
\usage{
nextA(b, d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{b}{ The current values for the normally distributed individual-level parameters - a gNP x gNIV matrix }	
   \item{d}{ The current variance-covariance matrix for the set of random parameters - a gNIV x gNIV matrix }
}
\author{
 Jeffrey Dumont
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
