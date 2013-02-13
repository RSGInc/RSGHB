\name{nextF}
\alias{nextF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Selects the next set of fixed coefficients.
}
\description{
 This function uses a Metropolis-Hastings function to generate a new set of fixed coefficients.
}
\usage{
nextF(p, f, b)
}
\arguments{
   \item{p}{ The current likelihood values for each individual - a vector  of length gNP }
   \item{f}{ The current value of the fixed (non-random) parameters - a vector of length gFIV }
   \item{b}{ The current values for the normally distributed individual-level parameters - a gNP x gNIV matrix }	
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
