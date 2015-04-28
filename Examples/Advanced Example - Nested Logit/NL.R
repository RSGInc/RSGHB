# ------------------
# Code for a Nested Logit Model
# This assumes that alt 1 and 2 are nested together and alternatives
# 3 and 4 are nested together
# 
# Jeff Dumont
#
# ------------------

library(RSGHB)



# ------------------
# DATA PREPARATION
# ------------------
# assumes that respondents are identified with a ID column
# also assumes that the data is sorted by respondent then experiment
choicedata <- read.table("Data_simulated.csv", sep = ",", header = TRUE)

# Specify any variables here that you'd like to use in the
# utility equations in the likelihood function below
# These can be any variables within the data or transformations of
# those variables
x1 <- choicedata$x1
x2 <- choicedata$x2
x3 <- choicedata$x3
x4 <- choicedata$x4
y1 <- choicedata$y1
y2 <- choicedata$y2
y3 <- choicedata$y3
y4 <- choicedata$y4

# The choice vectors
# Dummying coding the choice vector allows for easier coding of the 
# the likelihood calculations. So we will have one column for each 
# alternative in the design
choice1    <- (choicedata$choice == 1)
choice2    <- (choicedata$choice == 2)
choice3    <- (choicedata$choice == 3)
choice4    <- (choicedata$choice == 4)

# ------------------
# ESTIMATION CONTROL
# Setting control list for estimation
# ?doHB for more estimation options
# ------------------
modelname <- "Nested_Logit"     	# used for output

# Names for the normal variables
gVarNamesNormal <- c("Bx", "By")

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(1, 1)

# Names for the fixed variables
gVarNamesFixed <- c("lambda1", "lambda2")

# STARTING VALUES
svN <- c(0, 0)                 # for the random coefficients
FC  <- c(0.5, 0.5)             # for the fixed coefficients

# ITERATION SETTINGS
gNCREP    <- 10000  	  # Number of iterations to use prior to convergence
gNEREP    <- 10000 	       # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 250           # How frequently to print info about the iteration process

control <- list(
     modelname = modelname,
     gVarNamesNormal = gVarNamesNormal,
     gVarNamesFixed = gVarNamesFixed,
     svN = svN,
     FC = FC,
     gDIST = gDIST,
     gNCREP = gNCREP,
     gNEREP = gNEREP,
     gNSKIP = gNSKIP,
     gINFOSKIP = gINFOSKIP,
     write.results = TRUE,
     gSeed = 1987,
     nodiagnostics = TRUE, # Set this to FALSE to see initial model diagnostics
     verbose = FALSE       # Set this to TRUE to see real-time progress printed and plotted
)

# ------------------
# likelihood
# USE:     Calculates the likelihood of choice | B
#          Returns likelihood values for each observation
# NOTES:   This is where the bulk of the computation resides so coding this efficiently
#          is essential to reducing run time.
# ------------------
likelihood <- function(fc, b)
{

  cc <- 1
  Bx <- b[, cc]; cc <- cc + 1
  By <- b[, cc];
  
  cc <- 1
  lambda1 <- fc[cc]; cc <- cc + 1
  lambda2 <- fc[cc];
  
  v1 <- Bx * x1 + By * y1
  v2 <- Bx * x2 + By * y2
  v3 <- Bx * x3 + By * y3
  v4 <- Bx * x4 + By * y4
  
  # assuming alternatives 1 and 2 are nested together and
  # alternative 3 and 4 are nested together
  
  logsum1 <- log(exp(v1/lambda1) + exp(v2/lambda1))
  logsum2 <- log(exp(v3/lambda2) + exp(v4/lambda2))

  prob.nest <- (exp(lambda1 * logsum1) * (choice1 + choice2) + exp(lambda2 * logsum2) * (choice3 + choice4))/(exp(lambda1 * logsum1) + exp(lambda2 * logsum2))

  prob.withinnest <- (choice1 * exp(v1/lambda1) + choice2 * exp(v2/lambda1))/(exp(v1/lambda1)+exp(v2/lambda1)) + (choice3 * exp(v3/lambda2) + choice4 * exp(v4/lambda2))/(exp(v3/lambda2) + exp(v4/lambda2))
  
  p <- prob.nest * prob.withinnest
	
  return(p)
}

# Estimate the model
model <- doHB(likelihood, choicedata, control)
save(model, file = paste0(model$modelname, ".RData"))