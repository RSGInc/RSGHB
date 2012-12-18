\name{go}
\alias{go}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Starts the model estimation process
}
\description{
 After loading a model's control file into memory, the user can initiate the model estimation by calling the go() function. The function will perform some initial diagnostic tests via a call to \emph{checkModel} to look for common errors in specifying the model. Upon completion, the function will produce a number output files with the model parameters and convergence statistics. 
}
\details{
A number of output files will be generated.

\emph{A file} - The _A files contain the means of the underlying normal for each iteration used in defining the posterior distributions. 

\emph{B file, Bsd file} - The B file contains the average across iterations of the individual level draws for the normally distributed random parameters. The Bsd file provides the standard deviations of those individual draws. 

\emph{C file, Csd file} - The C file contains the average across iterations of the individual level transformed draws for the normally distributed random parameters. The Csd file provides the standard deviations of those individual draws. 

\emph{D file} - This file contains the variance-covariance matrix for the random coefficients for each iteration.

\emph{F file} - This file contains the set of fixed (non-random) coefficients for each iteration.

}
\usage{
go()
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
