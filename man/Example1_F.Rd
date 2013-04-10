\name{Example1_F}
\alias{Example1_F}
\docType{data}
\title{
An example F file
}
\description{
An F file from the first example in the vignette on using RSGHB
}
\usage{data(Example1_F)}
\format{
  A data frame with one row per iteration (after convergence) of the markov chains for the three parameters estimated.
  \describe{
    \item{\code{iteration}}{Iteration of the chain}
    \item{\code{ASC1}}{Estimate of ASC1 at iteration}
    \item{\code{BTime}}{Estimate of BTime at iteration}
    \item{\code{BCost}}{Estimate of BCost at iteration}
  }
}

\examples{
data(Example1_F)

plot(Example1_F[,2],main="Markov chain for ASC1",xlab="Iteration",ylab="Estimate",type="l")

}
\keyword{datasets}
\seealso{doHB}