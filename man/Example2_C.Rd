\name{Example2_C}
\alias{Example2_C}
\docType{data}
\title{
An example C file
}
\description{
A C file from the second example in the vignette on using RSGHB. The C file contains the average across iterations of the individual level draws for the random parameters.
}
\usage{data(Example2_C)}
\format{
  A data frame with one row per individual.
  \describe{
    \item{\code{Respondent}}{Decision maker identifier}
    \item{\code{RLH}}{Individual measure of model fit (Root Likelihood)}
    \item{\code{ASC1}}{Average of individual draws for parameter ASC1 }
    \item{\code{BTime}}{Average of individual draws for parameter BTime}
    \item{\code{BCost}}{Average of individual draws for parameter BCost}
  }
}

\examples{
data(Example2_C)

head(Example2_C)

}
\keyword{datasets}
\seealso{doHB}