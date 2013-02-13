\name{doHB}
\alias{doHB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Starts the model estimation process
}
\description{
The user can initiate the model estimation by calling the \emph{doHB()} function. The function will perform some initial diagnostic tests to look for common errors in specifying the model. Upon completion, the function will write a number of output files with the model parameters and convergence statistics to the user's working directory. 

The flexibility comes in allowing the user to specify the likelihood function directly instead of assuming predetermined model structures. Types of models that can be estimated with this code include the family of discrete choice models (Multinomial Logit, Mixed Logit, Nested Logit, Error Components Logit and Latent Class) as well ordered response models like ordered probit and ordered logit. In addition, the package allows for flexibility in specifying parameters as either fixed (non-varying across individuals) or random with continuous distributions. Parameter distributions supported include normal, positive log-normal, negative log-normal, positive truncated normal and the Johnson SB distribution.  

Kenneth Train's Matlab and Gauss code for doing hierarchical Bayesian estimation has served as the basis for a few of the functions included in this package. (See references below).
}

\usage{
doHB(likelihood_user,choicedata,control)
}
\arguments{
     \item{likelihood_user}{A function that returns likelihood values for each observation in your dataset. This function takes the current values  individual parameters and the set of fixed coefficients (f) and computes the likelihood of observing the data given those values.}
     \item{choicedata}{A data.frame of choice data to be used in estimation.}
     \item{control}{A set of estimation controls in list form. See below for more details.}
     }

\details{
There are a number of global variables that can be set to control the model estimation. Some need to specified directly in the model control file while others have default values that can be adjusted by the analyst if something other than the default is desired.

\bold{User-specified controls}

\emph{gVarNamesNormal} - A vector of charater-based names for the random coefficients. (Defaults to NULL) 

\emph{gVarNamesFixed} - A vector of character-based names for the fixed coefficients. (Defaults to NULL)

\emph{gDIST} - A vector of integers (1-5) which indicate which type of distribution should be applied to the random coefficients - 1 = Normal, 2 = Postive Log-Normal, 3 = Negative Log-Normal, 4 = Censored Normal, 5 = Johnson SB. There should be an element for each name in \emph{gVarNamesNormal}. (Defaults to NULL)

\emph{FC} - A vector of starting values for the fixed coefficients. There should be an element for each name in \emph{gVarNamesFixed}. (Defaults to NULL)

\emph{svN} - A vector of starting values for the means of the underlying normals for the random coefficients. There should be an element for each name in \emph{gVarNamesNormal}. (Defaults to NULL)

\emph{gNCREP} - Number of burn-in iterations to use prior to convergence. (Defaults to 100000)

\emph{gNEREP} - Number of iterations to keep for averaging after convergence has been reached. (Defaults to 100000)

\emph{gNSKIP} - Number of iterations in between retaining draws for averaging. (Defaults to 1)

\emph{gINFOSKIP} - Number of iterations in between printing/saving information about the iteration process. (Defaults to 250)

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

\emph{constraintsNorm} - This is a list of monotonic constraints to be applied during estimation. The structure of the constraints is c(param1number - inequality - param2number). For constraints relative to 0, use 0 instead of the param2number. For the inequality, use 1 for < and 2 for >. Example constraintsNorm <- list(c(5,1,0),c(6,1,5),c(7,1,6),c(8,1,7)) would constrain the 5th parameter < 0, the 6th parameter < 5th parameter, the 7th parameter < the 6th parameter, etc. (Defaults to NULL)

\emph{nodiagnostics} - If set to TRUE, the diagnostic report will not be reported to the screen with a prompt to continue. This makes batch processing easier to implement. (Defaults to FALSE)

\bold{Output files}

A number of output files will be generated.

\emph{A file} - The _A files contain the means of the underlying normal for each iteration used in defining the posterior distributions. 

\emph{B file, Bsd file} - The B file contains the average across iterations of the individual level draws for the normally distributed random parameters. The Bsd file provides the standard deviations of those individual draws. 

\emph{C file, Csd file} - The C file contains the average across iterations of the individual level transformed draws for the normally distributed random parameters. The Csd file provides the standard deviations of those individual draws. 

\emph{D file} - This file contains the variance-covariance matrix for the random coefficients for each iteration.

\emph{F file} - This file contains the set of fixed (non-random) coefficients for each iteration.

\emph{Log file} - This contains some statistics that can be used to understand if your model convergence has been reached.
}
\references{

Train, K. (2009) Discrete Choice Methods with Simulation. Cambridge University Press.

Train, K. and Sonnier G. (2005) \emph{Mixed Logit with Bounded Distributions of Correlated Partworths}, Applications of Simulation Methods in Environmental and Resource Economics. Edited by Anna Alberini and Riccardo Scarpa. http://elsa.berkeley.edu/~train/trainsonnier.pdf

Train, K. Original Gauss and Matlab code: http://elsa.berkeley.edu/Software/abstracts/train1006mxlhb.html 

}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
