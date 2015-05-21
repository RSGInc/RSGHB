\name{plotC}
\alias{plotC}
\title{
Plot Random Parameters
}
\description{
Plots the random parameters in an \code{RSGHB} model object. The \code{"C"} object contains the average across iterations for each set of individual level draws for the random parameters including the appropriate transformations.
}
\usage{
plotC(object, columns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An \code{RSGHB} model object.}
  \item{columns}{A vector of particular columns to plot. If \code{NULL}, plots all columns. (Defaults to \code{NULL})}
}
\author{
 Jeff Keller <jeff.keller@rsginc.com>
}
\seealso{
\code{\link{doHB}}, \code{\link{plotA}}, \code{\link{plotF}}, \code{\link{plotLog}}
}
\examples{
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
