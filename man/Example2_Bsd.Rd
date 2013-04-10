\name{Example2_Bsd}
\alias{Example2_Bsd}
\docType{data}
\title{
An example Bsd file
}
\description{
A Bsd file from the second example in the vignette on using RSGHB. The Bsd file contains the standard deviation across iterations of the individual level draws for the underlying normals for the random parameters.
}
\usage{data(Example2_Bsd)}
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
data(Example2_Bsd)

head(Example2_Bsd)

}
\keyword{datasets}
\seealso{doHB}