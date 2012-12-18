\name{createGlobalVariables}
\alias{createGlobalVariables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Create global variables used in estimation
}
\description{
 Creates necessary global variables used in estimation
}
\usage{
 createGlobalVariables()
}
\details{
 This function needs to be called first at the top of the user-specified model script. It creates a number of global variables which then could be adjusted based on the model specification desired. The global variables created are:
 
\emph{modelname} - The model name which is used for creating output files. (Defaults to paste("HBModel",round(runif(1)*10000000,0),sep=""))
     
\emph{gSIGDIG} - The number of significant digits for reporting purposes. (Defaults to 10)

\emph{priorVariance} - The amount of prior variance assumed. (Defaults to 2.0)       

\emph{degreesOfFreedom} - Additional degrees of freedom for the prior covariance matrix (not including the number of parameters. (Defaults to 5)           
     
\emph{rho} - The initial proportionality fraction for the jumping distribution  for the Metropolis-Hastings algorithm for the random coefficients. This fraction is adjusted by the program after each iteration to attain an acceptance rate of about 0.3 (Defaults to 0.1)       

\emph{rhoF} - The proportionality fraction for the jumping distribution for the Metropolis-Hastings algorithm for the fixed coefficients. Unlike \emph{rho}, this value is not adjusted as the markov chain proceeds. (Defaults to 0.0001)
          
\emph{gFULLCV} - A number that indicates if a full or independent covariance structure should be used for the random coefficients. A value of 1 indicated full and 0 for an independent structure. (Defaults to 1)
          
\emph{gMINCOEF} - A vector of minimums for the Johnson SB distributions. If Johnson SB is used, rach random coefficent needs an element but only the elements that correspond to a 5 in \emph{gDIST} are used. (Defaults to 0)

\emph{gMAXCOEF} -  Like \emph{gMINCOEF} but for the maximum of the Johnson SB distribution. (Defaults to 0)
     
\emph{gStoreDraws} - A boolean value to store the draws for the individual level coefficients. (Defaults to F)
     
\emph{gSeed} - The random seed. (Defaults to 0)
 
}
\references{
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
