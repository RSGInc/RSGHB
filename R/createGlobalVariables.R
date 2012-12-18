


createGlobalVariables <- function()
{
     modelname     <<- paste("HBModel",round(runif(1)*10000000,0),sep="")
     
     gSIGDIG       <<- 10        # number of significant digits for reporting purposes
     
     priorVariance <<- 2.0       # Train's original code use the number of parameters
     # Sawtooth uses a default of 2
     # Adjusting the prior variance upwards puts more emphasis on the 
     # fitting to the respondents' individual data
     
     degreesOfFreedom <<- 5      # additional degrees of freedom for the prior covariance matrix (not including the number of parameters
     # the higher the value, the greater the influence of the prior variance
     # sawtooth defaults to 5
     
     # Set the initial proportionality fraction for the jumping distribution 
     # This fraction is adjusted by the program in each iteration to attain
     # an acceptance rate of about .3 in the Metropolis-Hastings algorithm for the B's 
     
     rho              <<- 0.1       # this is used in the MH algorithm for the normal random coefficients
     rhoF             <<- 0.0001    # this is used in the MH algorithm for the fixed (non-random) coefficients
     
     # Want full or diagonal covariance matrix for random coefficients?
     # Set FULLCV=1 for full cov matrix, FULLCV=0 for diagonal matrix.
     
     gFULLCV <<- 1
     
     # If any element of gDIST is 5, then you must specify the max and min
     # for that coefficient. MIN and MAX each has NIV elements. 
     # Only the elements tat correspond to a 5 in gDIST are used. However,
     # all elements must be given real numbers
     
     gMINCOEF  <<- 0
     gMAXCOEF  <<- 0 
     
     gStoreDraws <<- F
     
     gSeed <<- 0 			# random seed
     
     
     # End user-specified GLOBAL VARIABLEs
     # ------------------
     
     
     
     
     # ------------------
     # FIXED GLOBAL VARIABLES
     # These should not be over-written by the analyst
     
     # variable initialization
     gNP           <<- 0         # number of individuals used in the model estimation
     gNOBS         <<- 0         # number of observations
     TIMES         <<- 0         # Number of observations for each person
     gIDS          <<- 0         # index map for individual to observation
     respIDs       <<- 0         # vector of unique identifiers
     A             <<- 0         # vector of means for the underlying normal distribution for the random coefficients
     B             <<- 0         # matrix of respondent specific coefficients
     Dmat          <<- 0         # var-covar for the set of preferences
     
     Choice        <<- 0         # vector of choices
     
     gNIV          <<- 0         # Number of random normal coefficients
     gFIV          <<- 0         # Number of fixed (non-random) coefficients
     
     starttime     <<- Sys.time()    # used to calculate seconds per iteration
     
     distNames     <<- c("N","LN+","LN-","TN","JSB")  # short names for the distributions
     # Normal, Postive Log-Normal, Negative Log-Normal, Positive Truncated Normal, Johnson SB
}