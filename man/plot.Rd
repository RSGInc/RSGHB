\name{plot.RSGHB}
\alias{plot}
\alias{plot.RSGHB}
\title{
 Plot RSGHB Model Object Components
}
\description{
This function plots various components of an \code{RSGHB} model object.
}
\usage{
\method{plot}{RSGHB}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An \code{RSGHB} model object.}
  \item{...}{Additional arguments passed to methods. See \code{Details}.}
}
\details{A \code{type} argument can be passed to control which component is plotted. If \code{type} is \code{"A"}, then the population means of the underlying normal distributions are plotted. If \code{type} is \code{"F"}, then the fixed parameters are plotted. If \code{type} is \code{"Log"} (the default), then the model statistics at every \code{gINFOSKIP}th iteration are plotted.

An error is returned if the \code{type} component is not found in \code{x}.
}
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
