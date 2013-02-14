# ------------------
# Code for a Latent Class Model with 2 classes
# 
# Jeff Dumont
#
# ------------------

library(RSGHB)

setwd("C:\\Work\\Code\\HB\\RSGHB.git\\Examples\\Advanced Example - Latent Class")     		    # working directory

# ------------------
# DATA PREPARATION
# ------------------
# assumes that respondents are identified with a ID column
# also assumes that the data is sorted by respondent then experiment
choicedata <- read.table("Data_simulated.csv",sep=",",header=T)

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
choice1    <- (choicedata$choice==1)
choice2    <- (choicedata$choice==2)
choice3    <- (choicedata$choice==3)
choice4    <- (choicedata$choice==4)

# ------------------
# ESTIMATION CONTROL
# Setting control list for estimation
# ?doHB for more estimation options
# ------------------

modelname <- "Latent_Class"		# used for output

# Names for the random variables
gVarNamesFixed <- c("theta1")

# Names for the random variables
gVarNamesNormal <- c("Bx_class1","By_class1","Bx_class2","By_class2")

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(1,1,1,1)

# STARTING VALUES
FC <- c(0)                  # for the fixed coefficients

svN <- c(0,0,0,0)           # for the random coefficients
                            # The selection of the mean here is important when working with non-normal distributions

# ITERATION SETTINGS
gNCREP    <- 20000  	  # Number of iterations to use prior to convergence
gNEREP    <- 10000 	       # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 250           # How frequently to print info about the iteration process

# CONTROL LIST TO PASS TO doHB
control <- list(
     modelname=modelname,
     gVarNamesNormal=gVarNamesNormal,
     gVarNamesFixed=gVarNamesFixed,
     gDIST=gDIST,
     svN=svN,
     FC=FC,
     gNCREP=gNCREP,
     gNEREP=gNEREP,
     gNSKIP=gNSKIP,
     gINFOSKIP=gINFOSKIP
)


# ------------------
# likelihood
# USE:     Calculates the likelihood of choice | B
#          Returns likelihood values for each observation
# NOTES:   This is where the bulk of the computation resides so coding this efficiently
#          is essential to reducing run time.
# ------------------
likelihood <- function(fc,b)
{

  cc <- 1
  Bx_class1  <- b[,cc];cc <- cc+1
  By_class1  <- b[,cc];cc <- cc+1
  Bx_class2  <- b[,cc];cc <- cc+1
  By_class2  <- b[,cc];cc <- cc+1
  
  cc <- 1
  theta1 <- fc[cc]
 
  p_class_membership1 <- exp(theta1)/(1+exp(theta1))
    
  v1_class1 <- Bx_class1 * x1 + By_class1 * y1 
  v2_class1 <- Bx_class1 * x2 + By_class1 * y2 
  v3_class1 <- Bx_class1 * x3 + By_class1 * y3
  v4_class1 <- Bx_class1 * x4 + By_class1 * y4

  v1_class2 <- Bx_class2 * x1 + By_class2 * y1 
  v2_class2 <- Bx_class2 * x2 + By_class2 * y2 
  v3_class2 <- Bx_class2 * x3 + By_class2 * y3
  v4_class2 <- Bx_class2 * x4 + By_class2 * y4

  expv1_class1 <- exp(v1_class1)
  expv2_class1 <- exp(v2_class1)
  expv3_class1 <- exp(v3_class1)
  expv4_class1 <- exp(v4_class1)
  
  expv1_class2 <- exp(v1_class2)
  expv2_class2 <- exp(v2_class2)
  expv3_class2 <- exp(v3_class2)
  expv4_class2 <- exp(v4_class2)
  
  p_class1 <- (expv1_class1*choice1 + expv2_class1*choice2 + expv3_class1*choice3 + expv4_class1*choice4) / (expv1_class1 + expv2_class1 + expv3_class1 + expv4_class1)
  
  p_class2 <- (expv1_class2*choice1 + expv2_class2*choice2 + expv3_class2*choice3 + expv4_class2*choice4) / (expv1_class2 + expv2_class2 + expv3_class2 + expv4_class2)
  

  p <- p_class1 * p_class_membership1 + p_class2 * (1-p_class_membership1)
  
  return(p)
}

# Estimate the model
doHB(likelihood, choicedata, control)