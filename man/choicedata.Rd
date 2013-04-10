\name{choicedata}
\alias{choicedata}
\docType{data}
\title{
A synthetic discrete choice dataset
}
\description{
A synthetic discrete choice dataset with each observation having two generic alternatives composed of two attributes. The data are panel in nature (as opposed to cross-sectional) with 4 choice tasks per decision maker.
}
\usage{data(choicedata)}
\format{
  A data frame with one row per choice task.
  \describe{
    \item{\code{ID}}{Decision maker identifier}
    \item{\code{choices}}{A vector of alternative chosen in each choice task}
    \item{\code{x1}}{Level of attribute1 on alternative 1}
    \item{\code{xprice}}{Level of price on alternative 1}
    \item{\code{y1}}{Level of attribute1 on alternative 2}
    \item{\code{yprice}}{Level of price on alternative 2}
  }
}
\details{
These choice data were synthetically derived to be used in generating examples of how RSGHB is used to estimate discrete choice models.
}
\examples{
data(choicedata)

# Frequency of alternatives chosen
table(choicedata$choices)

}
\keyword{datasets}
\seealso{doHB}