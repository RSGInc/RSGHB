\name{RSGHB-package}
\alias{RSGHB-package}
\alias{RSGHB}
\docType{package}
\title{
Functions for Hierarchical Bayesian Estimation: A Flexible Approach
}
\description{
 This package can be used to estimate models using a hierarchical Bayesian framework. The flexibility comes in allowing the user to specify the likelihood function directly instead of assuming predetermined model structures. Types of models that can be estimated with this code include the family of discrete choice models (Multinomial Logit, Mixed Logit, Nested Logit, and Latent Class) as well ordered response models like ordered probit and ordered logit. In addition, the package allows for flexibility in specifying parameters as either fixed (non-varying across individuals) or random with continuous distributions. Parameter distributions supported include normal, positive log-normal, negative log-normal, positive truncated normal and the Johnson SB distribution.
 
 A number of the functions that exist in this package had their origins in Kenneth Train's Matlab and Gauss code for doing hierarchical Bayesian estimation.
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
     
}

\author{
Jeffrey Dumont, RSG, Inc.
}
\usage{
checkmodel()
createGlobalVariables()
createBasicMNLFile()
createBasicMaxDiffFile()
go()
hb(a,b,d,f)
nextA(b,d)
nextB(a,b,d,p,f)
nextF(p,f,b)
nextD(a,b)
nextDind(a,b)
plotA(model)
plotF(model)
plotLog(model)
prepareModel()
trans(b)
vech(mat)
writeLog(r,p,a,b,f)
xpnd(vec)
}
\arguments{
   \item{a}{ The current means of the underlying normal distributions for the set of random parameters - a vector of length gNIV }
   \item{b}{ The current values for the normally distributed individual-level parameters - a gNP x gNIV matrix }	
   \item{d}{ The current variance-covariance matrix for the set of random parameters - a gNIV x gNIV matrix }
   \item{f}{ The current value of the fixed (non-random) parameters - a vector of length gFIV }
   \item{mat}{ A symmetric matrix }
   \item{model}{ A string containing the model's name }
   \item{p}{ The current likelihood values for each individual - a vector  of length gNP }
   \item{r}{ The current iteration of the Markov chain }
   \item{vec}{ A vector }
}
\references{
 Rossi, P., Allenby G. and McCulloch, R. (2005). Bayesian Statistics and Marketing. John Wiley & Sons.
 
 Train, K. (2009) Discrete Choice Methods with Simulation. Cambridge University Press.
}
\keyword{ package }
\seealso{
}
\examples{
}
