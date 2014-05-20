\name{plotC}
\alias{plotC}
\title{
 Plots the content of the C file
}
\description{
This function reads and plots the means of the content of the C file. The C file contains the average across iterations of the individual level draws for the random parameters including the appropriate transformations.
}
\usage{
plotC(model, columns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A string that corresponds to the model's name commonly stored in \code{modelname}. These are the characters to the left of the \code{"_"} in the C file name (model_C.csv)}
  \item{columns}{A vector of particular columns to plot. A value of \code{NULL} plots all columns. (defaults to \code{NULL})}
}
\author{
 Jeff Keller, RSG, Inc.
}
\seealso{
\code{\link{doHB}}
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
