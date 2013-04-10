\name{Example2_A}
\alias{Example2_A}
\docType{data}
\title{
An example A file
}
\description{
An A file from the second example in the vignette on using RSGHB. The A file contains the estimates of the sample-level means for the under-lying normal distributions of the random coefficients in the model.
}
\usage{data(Example2_A)}
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
data(Example2_A)

plot(Example2_A[,2],main="Markov chain for ASC1",xlab="Iteration",ylab="Estimate",type="l")

}
\keyword{datasets}
\seealso{doHB}