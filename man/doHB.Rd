\name{doHB}
\alias{doHB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate a Hierarchical Bayesian Model
}
\description{
The user can initiate the model estimation by calling the \code{doHB} function. The function will optionally perform initial diagnostic tests to look for common errors in specifying the model. Upon completion, the function will optionally write a number of output files with the model parameters and convergence statistics to the user's working directory. 

The flexibility comes in allowing the user to specify the likelihood function directly instead of assuming predetermined model structures. Types of models that can be estimated with this code include the family of discrete choice models (Multinomial Logit, Mixed Logit, Nested Logit, Error Components Logit and Latent Class) as well as ordered response models like ordered probit and ordered logit. In addition, the package allows for flexibility in specifying parameters as either fixed (non-varying across individuals) or random with continuous distributions. Parameter distributions supported include normal, positive/negative log-normal, positive/negative censored normal and the Johnson SB distribution.  

Kenneth Train's Matlab and Gauss code for doing Hierarchical Bayesian estimation has served as the basis for a few of the functions included in this package. (See references below).
}
\usage{
doHB(likelihood_user, choicedata, control = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{likelihood_user}{A function that returns likelihood values for each observation in your data set. This function must accept arguments \code{fc} and \code{b}, representing the fixed parameters and individual parameters, respectively, and computes the likelihood of observing the data given those values.}
     \item{choicedata}{A data.frame of choice data to be used in estimation. Minimally requires an 'ID' column associated with the vector of likelihoods returned by \code{likelihood_user}.}
     \item{control}{A list of estimation controls. See below for more details.}
     }
\details{
The \code{fc} argument to the \code{likelihood_user} function is a numeric vector of length \code{length(gVarNamesFixed)}. It is \code{NULL} if \code{gVarNamesFixed} is \code{NULL}.

The \code{b} argument to the \code{likelihood_user} function is a numeric matrix with \code{length(gVarNamesNormal)} columns and \code{length(likelihood_user(...))} rows. In other words, one column per random parameter and one row per choice task. It is \code{NULL} if \code{gVarNamesNormal} is \code{NULL}.

There are a number of global variables that can be set to control the model estimation. Some need to be specified directly in the model control list while others have default values that can be adjusted by the analyst if something other than the default is desired.

\bold{User-specified controls}

\emph{constraintsNorm} - A list of monotonic constraints to be applied during estimation. The structure of the constraints is \code{c(param1number, inequality, param2number)}. For constraints relative to \code{0}, use \code{0} instead of \code{param2number}. For \code{inequality}, use \code{1} for \code{<} and \code{2} for \code{>}.

For example \code{constraintsNorm = list(c(5,1,0), c(6,1,5), c(7,1,6))} would constrain the 5th parameter < 0, the 6th parameter < 5th parameter, and the 7th parameter < the 6th parameter. If \code{NULL}, no constraints are used. (Defaults to \code{NULL})

\emph{degreesOfFreedom} - Additional degrees of freedom for the prior variance-covariance matrix, not including the number of parameters. (Defaults to \code{5}) 

\emph{FC} - A vector of starting values for the fixed parameters. There should be an element for each name in \emph{gVarNamesFixed}. (Defaults to \code{rep(0, length(gVarNamesFixed))})

\emph{fixedA} - Fixes the means of the underlying normal distribution of random variables to certain values as opposed to estimating them. This would be important for example in an error components logit model or an integrated choice and latent variable model. The format for this input is a vector of length equal to the number of random parameters. Use \code{NA} for variables that should be estimated, e.g., \code{fixedA = c(NA, NA, NA, NA, NA, NA, NA, 0)}. In this case, the mean of the underlying normal for the 8th random variable would be fixed to \code{0}. If \code{NULL}, all means are estimated. (Defaults to \code{NULL})

\emph{fixedD} - Fixes the variance of the underlying normal distribution of the random variables to certain values as opposed to estimating them. This would be important for example in an integrated choice and latent variable model. The format for this input is a vector of length equal to the number of random parameters. Use \code{NA} for variables that should be estimated, e.g., \code{fixedD = c(NA, NA, NA, NA, NA, NA, NA, 1)}. In this case, the variance of the underlying normal for the 8th random variable would be fixed to \code{1}. If \code{NULL}, all variances are estimated. (Defaults to \code{NULL})

\emph{gDIST} - A vector of integers (1-6) that indicate which type of distribution should be applied to the random parameters. 1 = Normal, 2 = Postive Log-Normal, 3 = Negative Log-Normal, 4 = Positive Censored Normal, 5 = Negative Censored Normal, 6 = Johnson SB. There should be an element for each name in \code{gVarNamesNormal}. (Defaults to \code{rep(1, length(gVarNamesNormal))})

\emph{gFULLCV} - Indicates whether a full variance-covariance structure should be used for the random parameters. (Defaults to \code{TRUE})

\emph{gINFOSKIP} - Number of iterations between printing/plotting information about the iteration process. (Defaults to \code{250})

\emph{gMAXCOEF} - A vector of maximums for the Johnson SB distributions. If Johnson SB is used, each random coefficent needs an element but only the elements that correspond to a JSB in \code{gDIST} are used. (Defaults to \code{0})

\emph{gMINCOEF} - A vector of minimums for the Johnson SB distributions. If Johnson SB is used, each random coefficent needs an element but only the elements that correspond to a JSB in \code{gDIST} are used. (Defaults to \code{0})

\emph{gNCREP} - Number of burn-in iterations to use prior to convergence. (Defaults to \code{100000})

\emph{gNEREP} - Number of iterations to keep for averaging after convergence has been reached. (Defaults to \code{100000})

\emph{gNSKIP} - Number of iterations in between retaining draws for averaging. (Defaults to \code{1})

\emph{gSIGDIG} - The number of significant digits for reporting purposes. (Defaults to \code{10})

\emph{gVarNamesFixed} - A character vector of names for the fixed parameters. (REQUIRED)

\emph{gVarNamesNormal} - A character vector of names for the random parameters. (REQUIRED)
     
\emph{gSeed} - The random seed. (Defaults to \code{0})
     
\emph{gStoreDraws} - Whether to store the draws for the individual level parameters. Doing so can dramatically increase the memory usage of the model if there are a large number of individuals or draws. (Defaults to \code{FALSE})

\emph{modelname} - The model name which is used for creating output files. (Defaults to \code{"HBModel"})

\emph{nodiagnostics} - If \code{TRUE}, an initial diagnostic report and prompt is not reported to the screen. This makes batch processing more seamless. (Defaults to \code{FALSE})

\emph{priorVariance} - The prior variance assumed. Ignored if \code{pvMatrix} is not \code{NULL}. (Defaults to \code{2})

\emph{pvMatrix} - A custom prior variance-covariance matrix to be used in estimation. The prior variance-covariance matrix needs to be a matrix object and of the correct size: \code{length(gVarNamesNormal)} by \code{length(gVarNamesNormal)}. If provided, overrides \code{priorVariance}. (Defaults to \code{NULL})
     
\emph{rho} - The initial proportionality fraction for the jumping distribution of the Metropolis-Hastings algorithm for the random parameters. This fraction is adjusted after each iteration to target an acceptance rate of \code{targetAcceptanceNormal}. (Defaults to \code{0.1})       

\emph{rhoF} - The proportionality fraction for the jumping distribution for the Metropolis-Hastings algorithm for the fixed parameters. This fraction is adjusted after each iteration to target an acceptance rate of \code{targetAcceptanceFixed}. (Defaults to \code{0.0001})

\emph{svN} - A vector of starting values for the means of the underlying normals for the random parameters. There should be an element for each name in \code{gVarNamesNormal}. (Defaults to \code{rep(0, length(gVarNamesNormal))})

\emph{targetAcceptanceFixed} - The target acceptance rate in the Metropolis-Hastings algorithm for the fixed parameters. (Defaults to \code{0.3})

\emph{targetAcceptanceNormal} - The target acceptance rate in the Metropolis-Hastings algorithm for the random parameters. (Defaults to \code{0.3})

\emph{verbose} - Whether estimation information should be printed/plotted during the iteration process. (Defaults to \code{TRUE})
 
\emph{writeModel} - Indicates whether the model results should be written as a series of CSV files to the working directory upon estimation completion. (Defaults to \code{FALSE}, see \code{\link{writeModel}})

}
\value{
a model object of class \code{RSGHB}. Contains:

  \item{modelname}{A character string identifying the model.}
  \item{params.fixed}{A character vector naming the estimated fixed parameters.}
  \item{params.vary}{A character vector naming the estimated random parameters.}
  \item{distributions}{A character vector of assumed distributions for each random parameter.}
  \item{pv}{The prior variance-covariance matrix assumed for estimation.}
  \item{df}{Additional degrees of freedom in the model.}
  \item{gSIGDIG}{The number of significant digits used in the reporting of the model results.}
  \item{gNP}{The number of individuals in the model.}
  \item{gOBS}{The number of observations in the model.}
  \item{gNCREP}{The number of burn-in iterations used prior to convergence.}
  \item{gNEREP}{The number of iterations used for averaging.}
  \item{gSeed}{The random starting seed used.}
  \item{constraints}{A list of constraints. (see 'Details' above)}
  \item{iter.detail}{A data.frame of model statistics at every \code{gINFOSKIP}'th iteration.}
  \item{A}{A matrix containing the sample-level means of the underlying normals at each iteration. Is \code{NULL} if no random parameters were estimated.}
  \item{B, Bsd}{A matrix containing the mean individual-level draws across iterations for the underlying normals. The \code{Bsd} object provides the standard deviations of these individual draws. Is \code{NULL} if no random parameters were estimated.}
  \item{C, Csd}{A matrix containing the mean individual-level draws across iterations for the underlying normals while including the appropriate distribution transformations. The \code{Csd} object provides the standard deviations of these individual draws. Is \code{NULL} if no random parameters were estimated.}
  \item{D}{An array of the sample variance-covariance matrix for each iteration. Is \code{NULL} if no random parameters were estimated.}
  \item{F}{A matrix containing the set of fixed (non-random) parameters at each iteration. Is \code{NULL} if no fixed parameters were estimated.}
  \item{choices}{A vector of choices if provided.}
  \item{p}{A vector of probabilities at the mean values of \code{C} and \code{F}.}
  \item{ll0}{The initial log-likelihood given the starting values of \code{sVN} and \code{FC}.}
  \item{llf}{The final log-likelihood at the mean values of \code{C} and \code{F}.}
}
\references{

Train, K. (2009) Discrete Choice Methods with Simulation. Cambridge University Press.

Train, K. and Sonnier G. (2005) \emph{Mixed Logit with Bounded Distributions of Correlated Partworths}, Applications of Simulation Methods in Environmental and Resource Economics. Edited by Anna Alberini and Riccardo Scarpa. http://elsa.berkeley.edu/~train/trainsonnier.pdf

Train, K. Original Gauss and Matlab code: http://elsa.berkeley.edu/Software/abstracts/train1006mxlhb.html 
}
\author{
 Jeffrey Dumont <jeff.dumont@rsginc.com>, Jeff Keller <jeff.keller@rsginc.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.RSGHB}, \link{writeModel}}
}
\examples{

# Organize choicedata for modeling
data(choicedata)
tt1 <- choicedata$tt1
tt2 <- choicedata$tt2
toll2 <- choicedata$toll2
choice1 <- (choicedata$Choice==1)
choice2 <- (choicedata$Choice==2)

# The model likelihood function
likelihood <- function(fc, b) {  
     
     # Assign Beta vectors to named parameters for convenience
     cc    <- 1
     wtp1  <- b[, cc]; cc <- cc + 1
     price <- b[, cc]; cc <- cc + 1
     
     # Discrete choice utility in WTP-space
     v1 <-                 price * wtp1 * tt1
     v2 <- price * toll2 + price * wtp1 * tt2
     
     # Return the probability of choice
     p  <- (exp(v1)*choice1 + exp(v2)*choice2) / (exp(v1) + exp(v2))
     return(p)
}

# Estimation controls/settings
control <- list(
     modelname = "MNL_WTPSpace",
     gVarNamesNormal = c("WTP", "Price"),
     gNCREP = 500,
     gNEREP = 500,
     gINFOSKIP = 10,
     nodiagnostics = TRUE,
     gSeed = 1987
)

# Estimate the model
model <- doHB(likelihood, choicedata, control)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
