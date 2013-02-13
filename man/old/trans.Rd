\name{trans}
\alias{trans}
\title{
 Coefficient transformations
}
\description{
 The values stored in B are the individual-level coefficents for the underlying normal distributions. This function converts the values from the underlying normals to the final distributions. The distributional form for each random coefficient must be specified in vector called gDIST. A value of 1 returns a normal distribution, a value of 2 returns a positive log-normal, a value of 3 returns a negative log-normal, a value of 4 returns a positive truncated normal and a value of 5 returns a Johnson SB.
 
 Returns a matrix of transformed coefficients.

}
\usage{
trans(b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ The current values for the normally distributed individual-level parameters - a gNP x gNIV matrix }	
}
\details{
 If the desired distribution is Johnson SB then values for gMAXCOEFF and gMINCOEFF need to be defined.
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
