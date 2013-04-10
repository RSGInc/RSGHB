\name{Example2_B}
\alias{Example2_B}
\docType{data}
\title{
An example B file
}
\description{
A B file from the second example in the vignette on using RSGHB. The B file contains the average across iterations of the individual level draws for the underlying normals for the random parameters.
}
\usage{data(Example2_B)}
\format{
  A data frame with one row per individual.
  \describe{
    \item{\code{Respondent}}{Decision maker identifier}
    \item{\code{ASC1}}{Average of individual draws for parameter ASC1 }
    \item{\code{BTime}}{Average of individual draws for parameter BTime}
    \item{\code{BCost}}{Average of individual draws for parameter BCost}
  }
}

\examples{
data(Example2_B)

head(Example2_B)

}
\keyword{datasets}
\seealso{doHB}