# ------------------
# Code for a Multinomial Logit Model with Random Coefficients
# 
# Jeff Dumont
#
# ------------------

library(RSGHB)

setwd("C:\\Work\\Code\\HB\\RSGHB.git\\Examples\\Basic Example - MNL with monotonic constraints")     		    # working directory

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

brand1_1 <- choicedata$brand1==1
brand1_2 <- choicedata$brand1==2
brand1_3 <- choicedata$brand1==3
brand1_4 <- choicedata$brand1==4
brand1_5 <- choicedata$brand1==5

brand2_1 <- choicedata$brand2==1
brand2_2 <- choicedata$brand2==2
brand2_3 <- choicedata$brand2==3
brand2_4 <- choicedata$brand2==4
brand2_5 <- choicedata$brand2==5

brand3_1 <- choicedata$brand3==1
brand3_2 <- choicedata$brand3==2
brand3_3 <- choicedata$brand3==3
brand3_4 <- choicedata$brand3==4
brand3_5 <- choicedata$brand3==5

price1_1 <- choicedata$price1==1
price1_2 <- choicedata$price1==2
price1_3 <- choicedata$price1==3
price1_4 <- choicedata$price1==4
price1_5 <- choicedata$price1==5

price2_1 <- choicedata$price2==1
price2_2 <- choicedata$price2==2
price2_3 <- choicedata$price2==3
price2_4 <- choicedata$price2==4
price2_5 <- choicedata$price2==5

price3_1 <- choicedata$price3==1
price3_2 <- choicedata$price3==2
price3_3 <- choicedata$price3==3
price3_4 <- choicedata$price3==4
price3_5 <- choicedata$price3==5


# The choice vectors
# Dummying coding the choice vector allows for easier coding of the 
# the likelihood calculations. So we will have one column for each 
# alternative in the design
choice1    <- (choicedata$Choice==1)
choice2    <- (choicedata$Choice==2)
choice3    <- (choicedata$Choice==3)

# ------------------
# ESTIMATION CONTROL
# Setting control list for estimation
# ?doHB for more estimation options
# ------------------

modelname <- "Base_Constrained"     	# used for output

# Names for the random variables
gVarNamesNormal <- c("brand2","brand3","brand4","brand5","price2","price3","price4","price5")

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- rep(1,length(gVarNamesNormal))

# constraints for random parameters
# coding -> (param1number - inequality - param2number)
# inequality - 1 = <, 2 = >
constraintsNorm <- list(c(5,1,0),c(6,1,5),c(7,1,6),c(8,1,7))

# STARTING VALUES
svN <- rep(0,length(gVarNamesNormal))  # for the random coefficients
                                       # The selection of the mean here is important when working with non-normal distributions
# ITERATION SETTINGS
gNCREP    <- 20000  	  # Number of iterations to use prior to convergence
gNEREP    <- 20000 	       # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 250           # How frequently to print info about the iteration process

# CONTROL LIST TO PASS TO doHB
control <- list(
     modelname=modelname,
     gVarNamesNormal=gVarNamesNormal,
     gDIST=gDIST,
     svN=svN,
     gNCREP=gNCREP,
     gNEREP=gNEREP,
     gNSKIP=gNSKIP,
     gINFOSKIP=gINFOSKIP,
     constraintsNorm=constraintsNorm
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

  cc = 1
  bbrand2   <- b[,cc];cc=cc+1
  bbrand3   <- b[,cc];cc=cc+1
  bbrand4   <- b[,cc];cc=cc+1
  bbrand5   <- b[,cc];cc=cc+1
  
  bprice2   <- b[,cc];cc=cc+1
  bprice3   <- b[,cc];cc=cc+1
  bprice4   <- b[,cc];cc=cc+1
  bprice5   <- b[,cc];cc=cc+1
  
  v1 <- bbrand2 * brand1_2 + bbrand3 * brand1_3 + bbrand4 * brand1_4 + bbrand5 * brand1_5 + bprice2 * price1_2 + bprice3 * price1_3 + bprice4 * price1_4 + bprice5 * price1_5
  v2 <- bbrand2 * brand2_2 + bbrand3 * brand2_3 + bbrand4 * brand2_4 + bbrand5 * brand2_5 + bprice2 * price2_2 + bprice3 * price2_3 + bprice4 * price2_4 + bprice5 * price2_5
  v3 <- bbrand2 * brand3_2 + bbrand3 * brand3_3 + bbrand4 * brand3_4 + bbrand5 * brand3_5 + bprice2 * price3_2 + bprice3 * price3_3 + bprice4 * price3_4 + bprice5 * price3_5
 
  p  <- (exp(v1)*choice1 + exp(v2)*choice2 + exp(v3)*choice3) / (exp(v1) + exp(v2) + exp(v3))
  
  return(p)
}

# Estimate the model
doHB(likelihood, choicedata, control)
