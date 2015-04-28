# ------------------
# Code for a Ordered Probit with Random Coefficients and
# Fixed Thresholds
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
y1 <- choicedata$y1

# The choice vectors
# Dummying coding the choice vector allows for easier coding of the 
# the likelihood calculations. So we will have one column for each 
# alternative in the design
choice1    <- (choicedata$choice == 1)
choice2    <- (choicedata$choice == 2)
choice3    <- (choicedata$choice == 3)

# ------------------
# ESTIMATION CONTROL
# Setting control list for estimation
# ?doHB for more estimation options
# ------------------

modelname <- "Ordered_Probit"		# used for output

# Names for the random variables
gVarNamesNormal <- c("Bx", "By")

# Names for the random variables
gVarNamesFixed <- c("threshold1", "threshold2")

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(1, 1)

# STARTING VALUES
FC <- c(0, 1)             # for the fixed coefficients
svN <- c(0, 0)            # for the random coefficients
                          # The selection of the mean here is important when working with non-normal distributions

# ITERATION SETTINGS
gNCREP    <- 10000  	  # Number of iterations to use prior to convergence
gNEREP    <- 10000 	       # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 250           # How frequently to print info about the iteration process


# CONTROL LIST TO PASS TO doHB
control <- list(
     modelname = modelname,
     gVarNamesNormal = gVarNamesNormal,
     gDIST = gDIST,
     svN = svN,
     gVarNamesFixed = gVarNamesFixed,
     FC = FC,
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
  By <- b[, cc]; cc <- cc + 1
 
  cc <- 1
  threshold1 <- fc[cc]; cc <- cc + 1
  threshold2 <- fc[cc]; cc <- cc + 1
  
  v1 <- Bx * x1 + By * y1
 
  p <- (pnorm(threshold1 - v1) - pnorm(-300 - v1      )) * choice1 +
       (pnorm(threshold2 - v1) - pnorm(threshold1 - v1)) * choice2 + 
       (pnorm(300 - v1)        - pnorm(threshold2 - v1)) * choice3
  
  return(p)
}

# Estimate the model
model <- doHB(likelihood, choicedata, control)
save(model, file = paste0(model$modelname, ".RData"))