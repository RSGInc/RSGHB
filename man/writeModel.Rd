\name{writeModel}
\alias{writeModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write an RSGHB Model Object as Series of CSVs
}
\description{
Function for writing an RSGHB model object as a series of CSV files to the disk. Files are written in Sawtooth's CBC HB format to allow for easier integration with analysis processes built around that software.
}
\usage{
writeModel(object, writeDraws = TRUE, path = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an \code{RSGHB} model object.
}
  \item{writeDraws}{
whether to write out the individual draw files. (Defaults to \code{TRUE})
}
  \item{path}{
the folder location to write the files. (Defaults to \code{getwd()})
}
}
\details{
All output files are named with a \code{object[["modelname"]]} prefix. (E.g. modelname.log, modelname_C.csv)

If \code{writeDraws} is \code{TRUE}, a CSV file for each of \code{object[["gNP"]]} individuals is written to the disk. This may take a long time if \code{object[["gNEREP"]]} or \code{object[["gNP"]]} is large.
}
\value{
None, though a series of files is written to \code{path}:
  \item{modelname.log}{A log file containing summary model information and the estimation iteration detail.}
  \item{modelname_logPlot.png}{A faceted plot of all appropriate model statistics stored in the iteration detail of the model (\code{object[["iter.detail"]]}). Depending on the type of parameters estimated, some statistics may be \code{NULL} or \code{NA} and are omitted.}
  \item{modelname_A.csv}{If \code{object} was estimated with random parameters, contains the sample-level means of the underlying normals at each iteration.}
  \item{modelname_B.csv, modelname_Bsd.csv}{If \code{object} was estimated with random parameters, contains the mean individual-level draws across iterations for the underlying normals. The Bsd file provides the standard deviations of these individual draws.}
  \item{modelname_C.csv, modelname_Csd.csv}{If \code{object} was estimated with random parameters, contains the mean individual-level draws across iterations for the underlying normals while including the appropriate distribution transformations. The Csd file provides the standard deviations of these individual draws. These two files are equivalent to the conditional distributions from models estimated using Maximum Simulated Likelihood methods.}
  \item{modelname_D.csv}{If \code{object} was estimated with random parameters, contains a row-based representation of the sample variance-covariance matrix for each iteration.}
  \item{modelname_F.csv}{If \code{object} was estimated with fixed parameters, contains the set of fixed (non-random) parameters at each iteration in \code{object[["gNEREP"]]}.}
  \item{modelname_pvMatrix.csv}{If \code{object} was estimated with random parameters, contains the prior variance-covariance matrix that was assumed to estimate the model.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jeff Keller <jeff.keller@rsginc.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{doHB}},
\code{\link{plotLog}}
}
\examples{
     data(example_MNL)
     class(example_MNL)
     
     # writeModel(example_MNL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
