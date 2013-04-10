\name{Example2_D}
\alias{Example2_D}
\docType{data}
\title{
An example D file
}
\description{
A D file from the second example in the vignette on using RSGHB. The D file contains a row-based representation of the sample covariance for each iteration. 
}
\usage{data(choicedata)}
\format{
  A data frame with one row per iteration.
  \describe{
    \item{\code{iteration}}{Iteration of the chain}
    \item{\code{ASC1}}{Estimate of sample variance of ASC1 at iteration}
    \item{\code{ASC1xBTime}}{Estimate of sample covariance between ASC1 and BTime at iteration}
    \item{\code{ASC1xBCost}}{Estimate of sample covariance between ASC1 and BCost at iteration}
    \item{\code{BTime}}{Estimate of Sample Variance of BTime at iteration}
    \item{\code{BTimexBCost}}{Estimate of sample covariance between BTime and BCost at iteration}
    \item{\code{BCost}}{Estimate of Sample Variance of BCost at iteration}
    
  }
}
\examples{
data(Example2_D)

head(Example2_D)

# building the covariance matrix
covMat <- xpnd(colMeans(Example2_D[-1]))

rownames(covMat) <- c("ASC1","BTime","BCost")
colnames(covMat) <- c("ASC1","BTime","BCost")

covMat

}
\keyword{datasets}
\seealso{doHB}