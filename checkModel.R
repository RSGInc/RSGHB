checkModel <- function()
{
     
     passChecks=T
     
     # model checks to make sure things are coded properly
     
     # the user needs to specify some variables. lets make sure they exist.
     
     if(is.null(choicedata))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - choicedata - is undefined.\n")          
     }     
     if(is.null(gDIST))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - gDIST - is undefined.\n")          
     }
     if(is.null(gNCREP))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - gNCREP - is undefined.\n")
     }
     if(is.null(gNEREP))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - gNEREP - is undefined.\n")
     }
     if(is.null(gNSKIP))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - gNSKIP - is undefined.\n")
     }
     if(is.null(gINFOSKIP))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - gINFOSKIP - is undefined.\n")
     }
     if(is.null(likelihood))
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Variable - likelihood - is undefined.\n")
     }
     
     # check to see if we have some variables
     gNIV       <<- length(gVarNamesNormal)   # Number of Normal Independent Variables
     gFIV       <<- length(gVarNamesFixed)    # Number of Non-Random Independent Variables
     
     if(gNIV+gFIV==0)
     {
          passChecks=F    
          cat("\n********FATAL ERROR: Please specify at least one coefficient to be estimated in either in gVarNamesNormal or gVarNamesFixed.\n")
     }     
     # check to make sure that the number of distributions specified equals the 
     # the number of random coefficients in the model
     if(length(gDIST)!=gNIV)
     {
          passChecks <- F
          cat("\n********FATAL ERROR: The number of distributions specified in gDist doesn't equal the number of random coefficients in the model.\n")
     }
     # check to the see if the distributions specified exists in the set of allowable distributions
     for(d in gDIST)
     {
          if(d < 1 | d > length(distNames))
          {
               passChecks <- F
               cat("\n********FATAL ERROR: The specified distributions ", d, " in gDist do not exist\n")          
          }     
     }
     # check to see if we have enough starting values for both the random and fixed coefficients
     if(gNIV!=length(svN))
     {
          passChecks <- F    
          cat("\n********FATAL ERROR: There are too many/not enough starting values for the random coefficients. Check your sVN vector.\n")          
     }
     if(gFIV!=length(FC))
     {
          passChecks <- F    
          cat("\n********FATAL ERROR: There are too many/not enough starting values for the fixed coefficients. Check your FC vector.\n")          
     }
     # the software assumes that there exists a respondent identifier is called ID
     if(is.null(choicedata$ID))
     {
          passChecks <- F
          cat("\n********FATAL ERROR: Expecting to find a respondent identifier column called - ID - in your dataset. None found.\n")          
     }     
     # the software assumes that there exists a choice column called Choice
     if(is.null(choicedata$Choice))
     {
          passChecks <- F
          cat("\n********FATAL ERROR: Expecting to find a choice column called - Choice - in your dataset. None found.\n")          
     }    
          
     if(passChecks)
     {
          prepareModel()
          
          cat(rep("\n",128))
          cat("Diagnostic checks passed. ")
          cat("Please review before proceeding","\n\n")
          cat("Number of Individuals: ",gNP,"\n",sep="\t")
          cat("Number of Observations: ",gNOBS,"\n",sep="\t")
          cat("Prior variance: ", priorVariance,"\n",sep="\t")
          cat("Degrees of Freedom: ", degreesOfFreedom,"\n",sep="\t")
          cat("Avg. Number of Observations per Individual: ",gNOBS / gNP,"\n",sep="\t")
          cat("Initial Log-Likelihood: ",sum(log(likelihood(FC,B))),"\n",sep="\t")
          
          
   	  if(gFIV > 0)
	  {
	  	cat("Fixed parameters estimated:","\n")
	 	for(i in 1:gFIV)
	 	{
	 	     cat(gVarNamesFixed[i],"\n")
	 	}
	  }
	  if(gNIV>0)
	  {
		cat("Random Parameters estimated (Distribution):","\n")
                for(i in 1:gNIV)
                {
                      cat(gVarNamesNormal[i],"(",distNames[gDIST[i]],")","\n")
                }
          }
                  
          cat("\n","Choice Matrix","\n")
          
          choiceMatrix <- cbind(table(choice),round(prop.table(table(choice)),2))
          
          dimnames(choiceMatrix)[[2]] <- c("Count","%")
          print(choiceMatrix)
          
          cat("\n\n\n")
          rl <- readline("Enter 1 to Estimate Model, 2 to Stop Model")
          if(rl!=1)
          {
               passChecks=F
          }
     }
     return(passChecks)
}
