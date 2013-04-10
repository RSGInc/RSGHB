\name{choicedata}
\alias{choicedata}
\docType{data}
\title{
A synthetic discrete choice dataset
}
\description{
A synthetic discrete choice dataset with each observation having two generic alternatives composed of two attributes (travel time and toll). The data are panel in nature (as opposed to cross-sectional) with 8 choice tasks per decision maker.
}
\usage{data(choicedata)}
\format{
  A data frame with one row per choice task.
  \describe{
    \item{\code{ID}}{Decision maker identifier}
    \item{\code{thecount}}{Experiment number}
    \item{\code{tt1}}{Travel time for the first alternative}
    \item{\code{tt2}}{Travel time for the second alternative}
    \item{\code{toll2}}{Toll for the second alternative}
    \item{\code{asc1}}{Dummy for alternative specific constant}
    \item{\code{Choice}}{Choice made}
  }
}

\details{
These choice data were synthetically derived to be used in generating examples of how RSGHB is used to estimate discrete choice models.
}
\examples{
data(choicedata)

# Frequency of alternatives chosen
table(choicedata$Choice)

}
\keyword{datasets}
\seealso{doHB}