go <- function(nodiagnostics=F,control=list())
{          
     
     # ------------------
     # USER-SPECIFIED GLOBAL VARIABLES
     # These can be over-written by the analyst by specifying
     # entries in the control list
     if(is.null(control$modelname))
     {
          modelname <- paste("HBModel",round(runif(1)*10000000,0),sep="")
     }
     else
     {
          modelname <- control$modelname
     }
     
     if(is.null(control$constraintsNorm))
     {
          constraintsNorm <- NULL
     }
     else
     {
          constraintsNorm <- control$constraintsNorm
     }
     
     if(is.null(control$fixedA))
     {
          fixedA <- NULL
     }
     else
     {
          fixedA <- control$fixedA
     }
     
     # number of significant digits for reporting purposes
     if(is.null(control$gSIGDIG))
     {
          gSIGDIG <- 10 
     }
     else
     {
          gSIGDIG <- control$gSIGDIG
     }     
     
     # Sawtooth uses a default of 2
     # Adjusting the prior variance upwards puts more emphasis on the 
     # fitting to the respondents' individual data
     if(is.null(control$priorVariance))
     {
          priorVariance <- 2.0 
     }
     else
     {
          priorVariance <- control$priorVariance
     }      
     
     
     # additional degrees of freedom for the prior covariance matrix (not including the number of parameters
     # the higher the value, the greater the influence of the prior variance
     # sawtooth defaults to 5
     if(is.null(control$degreesOfFreedom))
     {
          degreesOfFreedom    <- 5 
     }
     else
     {
          degreesOfFreedom <- control$degreesOfFreedom
     }  
     
     
     # Set the initial proportionality fraction for the jumping distribution 
     # This fraction is adjusted by the program in each iteration to attain
     # an acceptance rate of about .3 in the Metropolis-Hastings algorithm for the B's
     
     # this is used in the MH algorithm for the normal random coefficients
     if(is.null(control$rho))
     {
          rho <- 0.1
     }
     else
     {
          rho <- control$rho
     }  
     
     # this is used in the MH algorithm for the fixed (non-random) coefficients
     if(is.null(control$rhoF))
     {
          rhoF <- 0.0001
     }
     else
     {
          rhoF <- control$rhoF
     }  
     
     # Want full or diagonal covariance matrix for random coefficients?
     # Set FULLCV=1 for full cov matrix, FULLCV=0 for diagonal matrix.
     if(is.null(control$gFULLCV))
     {
          gFULLCV <- 1
     }
     else
     {
          gFULLCV <- control$gFULLCV
     } 
     
     # if you want to store the individual draws
     if(is.null(control$gStoreDraws))
     {
          gStoreDraws <- F
     }
     else
     {
          gStoreDraws <- control$gStoreDraws
     } 
     
     # the random seed
     if(is.null(control$gSeed))
     {
          gSeed <- 0 
     }
     else
     {
          gSeed <- control$gSeed
     } 
     
     # End user-specified GLOBAL VARIABLEs     
     
     # ------------------
     # FIXED GLOBAL VARIABLES
     # These should not be over-written by the analyst
     
     # variable initialization
     gNP           <- 0         # number of individuals used in the model estimation
     gNOBS         <- 0         # number of observations
     TIMES         <- 0         # Number of observations for each person
     gIDS          <- 0         # index map for individual to observation
     respIDs       <- 0         # vector of unique identifiers
     A             <- 0         # vector of means for the underlying normal distribution for the random coefficients
     B             <- 0         # matrix of respondent specific coefficients
     Dmat          <- 0         # var-covar for the set of preferences
     
     Choice        <- 0         # vector of choices
     
     gNIV          <- length(gVarNamesNormal)         # Number of random normal coefficients
     gFIV          <- length(gVarNamesFixed)         # Number of fixed (non-random) coefficients
     
     starttime     <- Sys.time()    # used to calculate seconds per iteration
     
     distNames     <- c("N","LN+","LN-","TN","JSB")  # short names for the distributions
                         # Normal, Postive Log-Normal, Negative Log-Normal, Positive Truncated Normal, Johnson SB
     constraintLabels <- c("<",">")     
     
     
     if(checkModel(nodiagnostics))
     {
          r <- 1
          
          # Post Burn-in iterations          
          ma <- matrix(0,nrow=gNIV,ncol=gNEREP)
          md <- matrix(0,nrow=gNIV*(gNIV+1)/2,ncol=gNEREP)
          mb <- matrix(0,nrow=gNP,ncol=gNIV)
          mb.squared <- matrix(0,nrow=gNP,ncol=gNIV)
          mp <- matrix(0,nrow=gNP,ncol=gNEREP)
          mf <- matrix(0,nrow=gFIV,ncol=gNEREP)
          mc <- matrix(0,nrow=gNP,ncol=gNIV)
          mc.squared <- matrix(0,nrow=gNP,ncol=gNIV)      # variance calculation     
          storedDraws <- list()
          
          hb(A, B, Dmat, FC)
          
          if(gNIV > 0)
          {
               MA   <- data.frame(iteration=1:gNEREP,t(ma))
               MD   <- data.frame(iteration=1:gNEREP,t(md))
               MC   <- data.frame(id=respIDs,RLH=rowMeans(mp),mc/gNEREP)
               MCSd <- data.frame(id=respIDs,sqrt((mc.squared-mc^2/gNEREP)/gNEREP))
                    
               MB   <- data.frame(id=respIDs,mb/gNEREP)
               MBSd <- data.frame(id=respIDs,sqrt((mb.squared-mb^2/gNEREP)/gNEREP))
               names(MC) <- c("Respondent","RLH",gVarNamesNormal)
               names(MA) <- c("iteration",gVarNamesNormal)
               
               names(MCSd) <- c("Respondent",gVarNamesNormal)
               names(MB)   <- c("Respondent",gVarNamesNormal)
               names(MBSd) <- c("Respondent",gVarNamesNormal)               
          }
          if(gFIV>0)
          {   
               MF  <- data.frame(iteration=1:gNEREP,t(mf))
               names(MF) <- c("iteration",gVarNamesFixed)
          }          
          
          cat("Creating output files. Please be patient.","\n")
          
	     dev.copy(png,paste(modelname,"_markovChains.png",sep=""))
	     dev.off()
     
          if(gNIV > 0)
          {     
               write.table(MA,paste(modelname,"_A.csv",sep=""),sep=",",row.names=F)
               write.table(MD,paste(modelname,"_D.csv",sep=""),sep=",",row.names=F)
            	write.table(MB,paste(modelname,"_B.csv",sep=""),sep=",",row.names=F)
     	     write.table(MBSd,paste(modelname,"_Bsd.csv",sep=""),sep=",",row.names=F)
               write.table(MC,paste(modelname,"_C.csv",sep=""),sep=",",row.names=F)
               write.table(MCSd,paste(modelname,"_Csd.csv",sep=""),sep=",",row.names=F)
          }
               
          if(gFIV>0)
          {     
               write.table(MF,paste(modelname,"_F.csv",sep=""),sep=",",row.names=F)
          }
     
          if(gStoreDraws)
          {
               cat("Creating individual draw files.","\n")     
               for(i in 1:gNP)
               {
                    fn <- paste("Draws_",respIDs[i],".csv",sep="") 
                    
                    write.table(storedDraws[[i]],fn,sep=",",row.names=F,col.names=T)
                    
               }
          }
          
          cat("Output creation finished.","\n")
     }
}
