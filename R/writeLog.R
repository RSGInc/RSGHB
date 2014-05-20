writeLog <- function(r,p,a,b,d,f,env)
{
         
     cr <- rainbow(env$gNIV+env$gFIV)
     
     if(env$gNIV >0)
     {
          paramRMS <- sqrt(mean(apply(trans(b,env),1,function(x)x^2)))   
          avgVariance <- mean(apply(trans(b,env),1,var))
     }
     
     #Some printing progress to the screen
     
     cat(rep("\n",128))
     cat("-----------------------------------------------------------","\n")
     cat("Iteration: ", r, "\n",sep="\t")
     cat("-----------------------------------------------------------","\n")
     # Model Statistics
     if(env$gNIV>0){
          cat("RHO (Normal): ", env$rho, "\n")
          cat("Acceptance Rate (Normal): ", env$acceptanceRatePerc, "\n")  
     }
     if(env$gFIV>0){
          cat("RHO (Fixed): ", env$rhoF, "\n")     
          cat("Acceptance Rate (Fixed): ", env$acceptanceRateFPerc, "\n")     
     }
     cat("Log-Likelihood: ",signif(sum(log(p)),env$gSIGDIG),"\n",sep="\t")
     cat("RLH: ",signif(mean(p^(1/env$TIMES)),env$gSIGDIG),"\n",sep="\t")
     if(env$gNIV >0)
     {
          cat("Parameter RMS:",signif(paramRMS,env$gSIGDIG),"\n")
          cat("Avg. Variance:",signif(avgVariance,env$gSIGDIG),"\n")
     }
     
     cat("-----------------------------------------------------------","\n")
     
     # fixed coefficients
     if(env$gFIV > 0)
     {
          cat("Current values for fixed coefficients","\n")
          for(i in 1:env$gFIV)
          {
               cat(env$gVarNamesFixed[i],":",signif(f[i],env$gSIGDIG),"\n",sep="\t")
          }
     }
     
     # Normal Coefficients
     if(env$gNIV > 0)
     {
          cat("Current values for the population means of the random parameters","\n")
          meanBetas <- colMeans(trans(b,env))
          for(i in 1:env$gNIV)
          {
               cat(env$gVarNamesNormal[i],":",signif(t(a)[,i],env$gSIGDIG),"\n",sep="\t")
          }
     }
     
     # outputs to the screen time estimate of completion
     if(r>0)
     {
          cat("-----------------------------------------------------------","\n")
          
          tpi <- as.numeric(difftime(Sys.time(),env$starttime,units="sec"))/env$gINFOSKIP
          env$starttime <- Sys.time() # makes the forecast based on the most recent iterations
          
          cat("Time per iteration:",signif(tpi,1),"seconds")
          
          cat("\n")
          cat("Time to completion:",signif((env$gNCREP + env$gNEREP * env$gNSKIP - r)*tpi/60,4),"minutes")
          cat("\n")
     }
     cat("-----------------------------------------------------------","\n")
     
     cN <- F
     
     if(r==0) 
     {
          cN <- T
                    
          # creating and writing out the Log file header
          
          sink(paste(env$modelname,".log",sep=""))
          cat("Model Name:",env$modelname,"\n",sep="\t")
          cat("Number of individuals:",env$gNP,"\n",sep="\t")
          cat("Number of observations:",env$gNOBS,"\n",sep="\t")
          cat("Number of preliminary iterations:",env$gNCREP,"\n",sep="\t")
          cat("Number of draws used per individual:",env$gNEREP,"\n",sep="\t")
          cat("Random Seed:",env$gSeed,"\n",sep="\t")
          cat("Total iterations:",env$gNCREP + env$gNEREP,"\n",sep="\t")
          if(env$gNIV>0)
          {
               cat("Prior Variance:", env$priorVariance,"\n",sep="\t")
               cat("Degrees of Freedom:", env$degreesOfFreedom,"\n",sep="\t")
               if(env$useCustomPVMatrix)
               {
                    cat("Custom Prior Matrix Used: ", TRUE,"\n",sep="\t")
               }
          }
          cat("Number of parameters:",env$gNIV + env$gFIV,"\n",sep="\t")
          
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
          
          cat("\n\nEstimated:",format(Sys.time(), "%a %b %d %X %Y"),"\n",sep="\t")
          
          cat("-----------------------------------------------------------","\n")
          
          cat("\n")
          if(env$gNIV > 0)
          {
               if(env$gFIV==0)
               {
                    cat("Iteration","Log-Likelihood","RLH","Parameter RMS","Avg. Variance","Acceptance Rate (Normal)","\t")
               }
               
               if(env$gFIV > 0)
               {
                    cat("Iteration","Log-Likelihood","RLH","Parameter RMS","Avg. Variance","Acceptance Rate (Normal)","Acceptance Rate (Fixed)","\t")
               }
               
          }
          if(env$gNIV == 0)
          {
               cat("Iteration","Log-Likelihood","RLH","Acceptance Rate (Fixed)","\t")
          }          
          sink()	
          
          # setting up the plot for the alphas
          xmax <- env$gNCREP + env$gNEREP * env$gNSKIP+5000
          plot(r,0,main="Markov Chains",xlim=c(0,xmax),ylim=c(-5,5),pch=20,xlab="Iterations",ylab="Utility",axes=F)
          segments(env$gNCREP,-100,env$gNCREP,100,col="red",lty=2,lwd=2)
          segments(0,0,env$gNCREP + env$gNEREP * env$gNSKIP,0,col="gray",lty=1,lwd=1)
          axis(1,at=0:floor((env$gNCREP + env$gNEREP * env$gNSKIP)/5000)*5000)
          axis(2,at=-5:5)
          
     }	
     
     # plotting of the normals  to the screen
     if(env$gNIV >0)
     {
          for(i in 1:env$gNIV)
          {
               points(r,t(a)[i],pch=20,col=cr[i],cex=0.5)
          }
     }

     if(env$gFIV>0)
     {
          for(i in 1:env$gFIV)
          {
               points(r,f[i],pch=20,col=cr[env$gNIV + i],cex=0.5)
          }
     }
     
     Sys.sleep(0) # forces replotting
     
     # Sink iteration details to the log file.
     sink(paste(env$modelname,".log",sep=""),append=T)
     
     cat("\n")
     
     # writing to the log file
     if(env$gNIV > 0)
     {    
          if(env$gFIV==0)
          {
               cat(
                    r,
                    signif(sum(log(p)),env$gSIGDIG),
                    signif(mean(p^(1/env$TIMES)),env$gSIGDIG),
                    signif(paramRMS,env$gSIGDIG),
                    signif(avgVariance,env$gSIGDIG),
                    signif(env$acceptanceRatePerc,env$gSIGDIG),
                    sep="\t"
                    )
          }
          if(env$gFIV>0)
          {
               cat(
                    r,
                    signif(sum(log(p)),env$gSIGDIG),
                    signif(mean(p^(1/env$TIMES)),env$gSIGDIG),
                    signif(paramRMS,env$gSIGDIG),
                    signif(avgVariance,env$gSIGDIG),
                    signif(env$acceptanceRatePerc,env$gSIGDIG),
                    signif(env$acceptanceRateFPerc,env$gSIGDIG),
                    sep="\t"
               )
          }          
     }
     if(env$gNIV == 0)
     {
          
          cat(
               r,
               signif(sum(log(p)),env$gSIGDIG),
               signif(mean(p^(1/env$TIMES)),env$gSIGDIG),
               signif(env$acceptanceRateFPerc,env$gSIGDIG),
               sep="\t"
          )
     }
     
     
     sink()
     

     
}
