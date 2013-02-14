# ------------------
# Code for a Multinomial Logit Model with Fixed Coefficients
# 
# Jeff Dumont
#
# ------------------

library(RSGHB)

setwd("C:\\Work\\Code\\HB\\RSGHB.git\\Examples\\Basic Example - MNL with fixed parameters")  # working directory

# ------------------
# DATA PREPARATION
# ------------------

# assumes that respondents are identified with a ID column
# also assumes that the data is sorted by respondent then experiment
choicedata <- read.table("Data_simulated.dat",sep="\t",header=T)

# Specify any variables here that you'd like to use in the
# utility equations in the likelihood function below
# These can be any variables within the data or transformations of
# those variables
TT1     <- choicedata$tt1
TT2     <- choicedata$tt2
TOLL2   <- choicedata$toll2

# The choice vectors
# Dummying coding the choice vector allows for easier coding of the 
# the likelihood calculations. So we will have one column for each 
# alternative in the design
choice1    <- (choicedata$Choice==1)
choice2    <- (choicedata$Choice==2)

# ------------------
# ESTIMATION CONTROL
# Setting control list for estimation
# ?doHB for more estimation options
# ------------------

modelname <- "MNL"          # used for output

# Names for the fixed variables
gVarNamesFixed <- c("ASC1","BTime","BCost")

# STARTING VALUES
FC <- c(0,0,0)                  # for the fixed coefficients

# ITERATION SETTINGS
gNCREP    <- 30000  	  # Number of iterations to use prior to convergence
gNEREP    <- 20000 	       # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1		       # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 250           # How frequently to print info about the iteration process

# CONTROL LIST TO PASS TO doHB
control <- list(
     modelname=modelname,
     gVarNamesFixed=gVarNamesFixed,
     FC=FC,
     gNCREP=gNCREP,
     gNEREP=gNEREP,
     gNSKIP=gNSKIP,gINFOSKIP=gINFOSKIP
)

# ------------------
# likelihood
# USE:     Calculates the likelihood of choice | B
#          Returns likelihood values for each observation
# NOTES:   This is where the bulk of the computation resides so coding this efficiently
#	      is essential to reducing run time.
# ------------------
likelihood <- function(fc,b)
{

  cc <- 1
  ASC1   <- fc[cc];cc=cc+1
  Btime  <- fc[cc];cc=cc+1
  Btoll  <- fc[cc];cc=cc+1  
  
  v1 <- ASC1       + Btime * TT1                   
  v2 <-              Btime * TT2 + Btoll * TOLL2   
 
  p  <- (exp(v1)*choice1 + exp(v2)*choice2) / (exp(v1) + exp(v2))
	
  return(p)
}

# Estimate the model
doHB(likelihood, choicedata, control)
