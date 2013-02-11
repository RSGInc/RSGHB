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

There are a number of global variables that can be set to control the model estimation. Some need to specified directly in the model control file while others have default values that can be adjusted by the analyst if something other than the default is desired.

\bold{User-specified with no defaults}

\emph{gVarNamesNormal} - A vector of charater-based names for the random coefficients 

\emph{gVarNamesFixed} - A vector of character-based names for the fixed coefficients

\emph{gDIST} - A vector of integers (1-5) which indicate which type of distribution should be applied to the random coefficients - 1 = Normal, 2 = Postive Log-Normal, 3 = Negative Log-Normal, 4 = Censored Normal, 5 = Johnson SB. There should be an element for each name in \emph{gVarNamesNormal}.

\emph{FC} - A vector of starting values for the fixed coefficients. There should be an element for each name in \emph{gVarNamesFixed}.

\emph{svN} - A vector of starting values for the random coefficients. There should be an element for each name in \emph{gVarNamesNormal}.

\emph{gNCREP} - Number of burn-in iterations to use prior to convergence.

\emph{gNEREP} - Number of iterations to keep for averaging after convergence has been reached.

\emph{gNSKIP} - Number of iterations in between retaining draws for averaging.

\emph{gINFOSKIP} - Number of iterations in between printing/saving information about the iteration process.

\emph{likelihood} - A function that takes the current values of the normally distributed individual parameters (b) and the set of fixed coefficients (f) and computes the likelihood of observing the data given those values.

\bold{User-specified with defaults}

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
     
A number of output files will be generated.

\emph{A file} - The _A files contain the means of the underlying normal for each iteration used in defining the posterior distributions. 

\emph{B file, Bsd file} - The B file contains the average across iterations of the individual level draws for the normally distributed random parameters. The Bsd file provides the standard deviations of those individual draws. 

\emph{C file, Csd file} - The C file contains the average across iterations of the individual level transformed draws for the normally distributed random parameters. The Csd file provides the standard deviations of those individual draws. 

\emph{D file} - This file contains the variance-covariance matrix for the random coefficients for each iteration.

\emph{F file} - This file contains the set of fixed (non-random) coefficients for each iteration.

}

\usage{
go(nodiagnostics=F)
}
\arguments{
     \item{nodiagnostics}{If set to TRUE no diagnostic report will be presented to the screen. Diagnostic checks will still be performed.}
     }
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
