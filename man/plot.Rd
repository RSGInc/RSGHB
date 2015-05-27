\name{plot.RSGHB}
\alias{plot}
\title{
 Plot RSGHB Model Object Components
}
\description{
This function plots various components of an \code{RSGHB} model object.
}
\usage{
plot(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An \code{RSGHB} model object.}
  \item{...}{Additional arguments.}
}
\details{A \code{type} argument can be passed to control which component is plotted. If \code{type} is \code{"A"} (the default), then the population means of the underlying normal distributions are plotted. If \code{type} is \code{"F"}, then the fixed parameters are plotted. If \code{type} is \code{"Log"}, then the model statistics at every \code{gINFOSKIP}th iteration are plotted.}

An error is returned if the \code{type} component is not found in \code{x}.
\author{
 Jeff Keller <jeff.keller@rsginc.com>, Jeffrey Dumont <jeff.dumont@rsginc.com>
}
\seealso{
\code{\link{doHB}}
}
\examples{

# plot(model, type = "A")
# plot(model, type = "F")
# plot(model, type = "Log")

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
