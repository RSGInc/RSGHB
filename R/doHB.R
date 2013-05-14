doHB <- function(likelihood_user,choicedata,control=list())
{          
     
     # turns the user specified likelihood function into something RSGHB
     # can use
     likelihood <- function(fc,b,env)
     {
          if(env$gNIV>0)
          {
               gIDS <- env$gIDS 
               C    <- trans(b,env)
               
               if(env$gNIV > 1)
               {
                    C    <- C[gIDS,]
               }
               
               if(env$gNIV==1)
               {                    
                    C    <- matrix(C[gIDS],ncol=env$gNIV)
               }
          }

          p <- likelihood_user(fc,C)
          
          p <- replace(p,is.na(p),1e-323)
          
          p0 <- rep(1,env$gNP)

          p0 <- .C("aggregation", 
             as.double(env$gIDS), 
             as.double(env$gNOBS),
             as.double(env$gNP),
             as.double(p),
             as.double(1:env$gNP),
             as.double(p0)
          )[[6]]
          
          p0 <- replace(p0,p0<1e-323,1e-323)
               
          return(p0)
     }
     
     # ------------------
     # USER-SPECIFIED GLOBAL VARIABLES
     # These can be over-written by the analyst by specifying
     # entries in the control list
     if(is.null(control[["modelname"]]))
     {
          modelname <- paste("HBModel",round(runif(1)*10000000,0),sep="")
     } else
     {
          modelname <- control[["modelname"]]
     }

     if(is.null(control[["gVarNamesNormal"]]))
     {
          gVarNamesNormal <- c()
     } else
     {
          gVarNamesNormal <- control[["gVarNamesNormal"]]
     }
     
     if(is.null(control[["gVarNamesFixed"]]))
     {
          gVarNamesFixed <- c()
     } else
     {
          gVarNamesFixed <- control[["gVarNamesFixed"]]
     }
     
     if(is.null(control[["gDIST"]]))
     {
          gDIST <- c()
     } else
     {
          gDIST <- control[["gDIST"]]
     }
     
     if(is.null(control[["FC"]]))
     {
          FC <- c()
     } else
     {
          FC <- control[["FC"]]
     }
     
     if(is.null(control[["svN"]]))
     {
          svN <- c()
     } else
     {
          svN <- control[["svN"]]
     }
     
     if(is.null(control[["gNCREP"]]))
     {
          gNCREP <- 100000
     } else
     {
          gNCREP <- control[["gNCREP"]]
     }
     
     if(is.null(control[["gNEREP"]]))
     {
          gNEREP <- 100000
     } else
     {
          gNEREP <- control[["gNEREP"]]
     }
     
     if(is.null(control[["gNSKIP"]]))
     {
          gNSKIP <- 1
     } else
     {
          gNSKIP <- control[["gNSKIP"]]
     }
     
     if(is.null(control[["gINFOSKIP"]]))
     {
          gINFOSKIP <- 250
     } else
     {
          gINFOSKIP <- control[["gINFOSKIP"]]
     }
     
     if(is.null(control[["constraintsNorm"]]))
     {
          constraintsNorm <- NULL
     } else
     {
          constraintsNorm <- control[["constraintsNorm"]]
     }
     
     if(is.null(control[["fixedA"]]))
     {
          fixedA <- NULL
     } else
     {
          fixedA <- control[["fixedA"]]
     }
     
     if(is.null(control[["fixedD"]]))
     {
          fixedD <- NULL
     } else
     {
          fixedD <- control[["fixedD"]]
     }

     if(is.null(control[["nodiagnostics"]]))
     {
          nodiagnostics <- F
     } else
     {
          nodiagnostics <- control[["nodiagnostics"]]
     }
     
     # number of significant digits for reporting purposes
     if(is.null(control[["gSIGDIG"]]))
     {
          gSIGDIG <- 10 
     } else
     {
          gSIGDIG <- control[["gSIGDIG"]]
     }     
     
     # Sawtooth uses a default of 2
     # Adjusting the prior variance upwards puts more emphasis on the 
     # fitting to the respondents' individual data
     if(is.null(control[["priorVariance"]]))
     {
          priorVariance <- 2.0 
     } else
     {
          priorVariance <- control[["priorVariance"]]
     }      
     
     
     # additional degrees of freedom for the prior covariance matrix (not including the number of parameters
     # the higher the value, the greater the influence of the prior variance
     # sawtooth defaults to 5
     if(is.null(control[["degreesOfFreedom"]]))
     {
          degreesOfFreedom    <- 5 
     } else
     {
          degreesOfFreedom <- control[["degreesOfFreedom"]]
     }  
     
     
     # Set the initial proportionality fraction for the jumping distribution 
     # This fraction is adjusted by the program in each iteration to attain
     # an acceptance rate of about .3 in the Metropolis-Hastings algorithm for the B's
     
     # this is used in the MH algorithm for the normal random coefficients
     if(is.null(control[["rho"]]))
     {
          rho <- 0.1
     } else
     {
          rho <- control[["rho"]]
     }  
     
     # this is used in the MH algorithm for the fixed (non-random) coefficients
     if(is.null(control[["rhoF"]]))
     {
          rhoF <- 0.0001
     } else
     {
          rhoF <- control[["rhoF"]]
     }  
     
     # Want full or diagonal covariance matrix for random coefficients?
     # Set FULLCV=1 for full cov matrix, FULLCV=0 for diagonal matrix.
     if(is.null(control[["gFULLCV"]]))
     {
          gFULLCV <- 1
     } else
     {
          gFULLCV <- control[["gFULLCV"]]
     } 
     
     # if you want to store the individual draws
     if(is.null(control[["gStoreDraws"]]))
     {
          gStoreDraws <- F
     } else
     {
          gStoreDraws <- control[["gStoreDraws"]]
     } 
     
     # the random seed
     if(is.null(control[["gSeed"]]))
     {
          gSeed <- 0 
     } else
     {
          gSeed <- control[["gSeed"]]
     } 
     
     # used in the johnson SB distribution
     if(is.null(control[["gMINCOEF"]]))
     {
          gMINCOEF <- 0 
     } else
     {
          gMINCOEF <- control[["gMINCOEF"]]
     } 
     
     if(is.null(control[["gMAXCOEF"]]))
     {
          gMAXCOEF <- 0 
     } else
     {
          gMAXCOEF <- control[["gMAXCOEF"]]
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
     
     acceptanceRateF <- 0
     rhoFadj         <- 1e-5
     
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
               ma   <- cbind(iteration=1:gNEREP,t(ma))
               md   <- cbind(iteration=1:gNEREP,t(md))
               
               mcsd <- cbind(id=respIDs,sqrt((mc.squared-mc^2/gNEREP)/gNEREP))
               mc   <- cbind(id=respIDs,RLH=rowMeans(mp),mc/gNEREP)
               
               mbsd <- cbind(id=respIDs,sqrt((mb.squared-mb^2/gNEREP)/gNEREP))
               mb   <- cbind(id=respIDs,mb/gNEREP)
               colnames(mc) <- c("Respondent","RLH",gVarNamesNormal)
               colnames(ma) <- c("iteration",gVarNamesNormal)
               
               colnames(mcsd) <- c("Respondent",gVarNamesNormal)
               colnames(mb)   <- c("Respondent",gVarNamesNormal)
               colnames(mbsd) <- c("Respondent",gVarNamesNormal)               
          }
          if(gFIV>0)
          {   
               mf  <- cbind(iteration=1:gNEREP,t(mf))
               colnames(mf) <- c("iteration",gVarNamesFixed)
          }          
          
          cat("Creating output files. Please be patient.","\n")
          
	     dev.copy(png,paste(modelname,"_markovChains.png",sep=""))
	     dev.off()
     
          if(gNIV > 0)
          {     
               write.table(ma,paste(modelname,"_A.csv",sep=""),sep=",",row.names=F)
               write.table(md,paste(modelname,"_D.csv",sep=""),sep=",",row.names=F)
            	write.table(mb,paste(modelname,"_B.csv",sep=""),sep=",",row.names=F)
     	     write.table(mbsd,paste(modelname,"_Bsd.csv",sep=""),sep=",",row.names=F)
               write.table(mc,paste(modelname,"_C.csv",sep=""),sep=",",row.names=F)
               write.table(mcsd,paste(modelname,"_Csd.csv",sep=""),sep=",",row.names=F)
          }
               
          if(gFIV>0)
          {     
               write.table(mf,paste(modelname,"_F.csv",sep=""),sep=",",row.names=F)
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
