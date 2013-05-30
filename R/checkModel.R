checkModel <- function(nodiagnostics=F, env=parent.frame())
{
     
     passChecks=TRUE

     # model checks to make sure things are coded properly
     
     # the user needs to specify some variables. lets make sure they exist.
     
     if(is.null(env$gDIST)&env$gNIV>0)
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: Variable - gDIST - is undefined.\n")          
     }
     if(is.null(env$gNCREP))
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: Variable - gNCREP - is undefined.\n")
     }
     if(is.null(env$gNEREP))
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: Variable - gNEREP - is undefined.\n")
     }
     if(is.null(env$gNSKIP))
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: Variable - gNSKIP - is undefined.\n")
     }
     if(is.null(env$gINFOSKIP))
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: Variable - gINFOSKIP - is undefined.\n")
     }
     if(is.null(env$likelihood))
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: The likelihood function is undefined.\n")
     }
     
     if(env$gNIV+env$gFIV==0)
     {
          passChecks=FALSE    
          cat("\n********FATAL ERROR: Please specify at least one coefficient to be estimated in either in gVarNamesNormal or gVarNamesFixed.\n")
     }     
     # check to make sure that the number of distributions specified equals the 
     # the number of random coefficients in the model
     if(length(env$gDIST)!=env$gNIV)
     {
          passChecks <- FALSE
          cat("\n********FATAL ERROR: The number of distributions specified in gDist doesn't equal the number of random coefficients in the model.\n")
     }
     # check to the see if the distributions specified exists in the set of allowable distributions
     if(env$gNIV > 0)
     {
          for(d in env$gDIST)
          {
               if(d < 1 | d > length(env$distNames))
               {
                    passChecks <- FALSE
                    cat("\n********FATAL ERROR: The specified distributions ", d, " in gDist do not exist\n")          
               }     
          }
     }
     # check to see if we have enough starting values for both the random and fixed coefficients
     if(env$gNIV!=length(env$svN))
     {
          passChecks <- FALSE    
          cat("\n********FATAL ERROR: There are too many/not enough starting values for the random coefficients. Check your sVN vector.\n")          
     }
     if(env$gFIV!=length(env$FC))
     {
          passChecks <- FALSE    
          cat("\n********FATAL ERROR: There are too many/not enough starting values for the fixed coefficients. Check your FC vector.\n")          
     }
     # the software assumes that there exists a respondent identifier is called ID
     if(is.null(env$choicedata$ID))
     {
          passChecks <- FALSE
          cat("\n********FATAL ERROR: Expecting to find a respondent identifier column called - ID - in your dataset. None found.\n")          
     }         

     # the software needs the data sorted by ID
     if(sum(sort(env$choicedata$ID)==env$choicedata$ID)!=length(env$choicedata$ID))
     {
          passChecks <- FALSE    
          cat("\n********FATAL ERROR: The choice data is not sorted by ID.\n")               
     }
     
     #Make sure the output files don't already exist
     if(any(file.exists(paste0(modelname, c(".log", "_A.csv", "_B.csv", "_Bsd.csv", "_C.csv", "_Csd.csv", "_D.csv", "_F.csv")))))
     {
          passChecks <- FALSE    
          cat("\n********FATAL ERROR: It appears output files for a model run with this modelname already exist.\n")             
     }
     
     if(passChecks)
     {
          prepareModel(env)
          
          cat(rep("\n",128))
          cat("Diagnostic checks passed. ")
          cat("Please review before proceeding","\n\n")
          cat("Number of Individuals: ",env$gNP,"\n",sep="\t")
          cat("Number of Observations: ",env$gNOBS,"\n",sep="\t")
          cat("Prior variance: ", env$priorVariance,"\n",sep="\t")
          cat("Degrees of Freedom: ", env$degreesOfFreedom,"\n",sep="\t")
          cat("Avg. Number of Observations per Individual: ",env$gNOBS / env$gNP,"\n",sep="\t")
          cat("Initial Log-Likelihood: ",sum(log(env$likelihood(env$FC,env$B,env))),"\n",sep="\t")
          
          
   	     if(env$gFIV > 0)
	     {
	  	     cat("Fixed parameters estimated:","\n")
	 	     for(i in 1:env$gFIV)
	 	     {
	 	          cat(env$gVarNamesFixed[i],"\n")
	 	     }
	     }
          
	     if(env$gNIV>0)
	     {
		     cat("Random Parameters estimated (Distribution):","\n")
               for(i in 1:env$gNIV)
               {
                    cat(env$gVarNamesNormal[i],"(",env$distNames[env$gDIST[i]],")","\n")
               }
          }
          
          if(!is.null(env$constraintsNorm))
          {
               cat("Constraints applied to random parameters (param1 - inequality - param2):","\n")
               for(i in 1:length(env$constraintsNorm))
               {
                    if(env$constraintsNorm[[i]][3]==0)
                         cat(env$gVarNamesNormal[env$constraintsNorm[[i]][1]],env$constraintLabels[env$constraintsNorm[[i]][2]],0,"\n")
                    if(env$constraintsNorm[[i]][3]!=0)
                         cat(env$gVarNamesNormal[env$constraintsNorm[[i]][1]],env$constraintLabels[env$constraintsNorm[[i]][2]],env$gVarNamesNormal[env$constraintsNorm[[i]][3]],"\n")
               }
               
          }
          
          if(!is.null(env$Choice))
          {
               cat("\n","Choice Matrix","\n")
               choiceMatrix <- cbind(table(env$Choice),round(prop.table(table(env$Choice)),2))
          
               dimnames(choiceMatrix)[[2]] <- c("Count","%")
               print(choiceMatrix)
          }
          
          cat("\n\n\n")
          if(!nodiagnostics)
          {
               rl <- readline("Enter 1 to Estimate Model, 2 to Stop Model")
               if(rl!=1)
               {
                    passChecks=FALSE
               }
          }
     }
     return(passChecks)
}
