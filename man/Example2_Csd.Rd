\name{Example2_Csd}
\alias{Example2_Csd}
\docType{data}
\title{
An example Csd file
}
\description{
A Csd file from the second example in the vignette on using RSGHB. The Csd file contains the standard deviation across iterations of the individual level draws for the random parameters.
}
\usage{data(Example2_Csd)}
\format{
  A data frame with one row per individual.
  \describe{
    \item{\code{Respondent}}{Decision maker identifier}
    \item{\code{ASC1}}{Standard deviation of individual draws for parameter ASC1 }
    \item{\code{BTime}}{Standard deviation of individual draws for parameter BTime}
    \item{\code{BCost}}{Standard deviation of individual draws for parameter BCost}
  }
}

\examples{
data(Example2_Csd)

head(Example2_Csd)

}
\keyword{datasets}
\seealso{doHB}