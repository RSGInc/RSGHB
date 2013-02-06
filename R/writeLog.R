writeLog <- function(r,p,a,b,d,fc)
{
         
     cr <- rainbow(gNIV+gFIV)
     
     if(gNIV >0)
     {
          paramRMS <- sqrt(.Internal(mean(apply(trans(b),1,function(x)x^2))))    #use .Internal for speed
          avgVariance <- .Internal(mean(apply(trans(b),1,var)))
     }
     
     #Some printing progress to the screen
     
     cat(rep("\n",128))
     cat("-----------------------------------------------------------","\n")
     cat("Iteration: ", r, "\n",sep="\t")
     cat("-----------------------------------------------------------","\n")
     # Model Statistics
     if(gNIV>0){
          cat("RHO (Normal): ", rho, "\n")
     }
     if(gFIV>0){
          cat("RHO (Fixed): ", rhoF, "\n")     
     }
     cat("Log-Likelihood: ",signif(sum(log(p)),gSIGDIG),"\n",sep="\t")
     cat("RLH: ",signif(mean(p^(1/TIMES)),gSIGDIG),"\n",sep="\t")
     if(gNIV >0)
     {
          cat("Parameter RMS:",signif(paramRMS,gSIGDIG),"\n")
          cat("Avg. Variance:",signif(avgVariance,gSIGDIG),"\n")
     }
     
     cat("-----------------------------------------------------------","\n")
     
     # fixed coefficients
     if(gFIV > 0)
     {
          cat("Current values for fixed coefficients","\n")
          for(i in 1:gFIV)
          {
               cat(gVarNamesFixed[i],":",signif(fc[i],gSIGDIG),"\n",sep="\t")
          }
     }
     
     # Normal Coefficients
     if(gNIV > 0)
     {
          cat("Current values for the means of the underlying normals\nfor the random parameters","\n")
          for(i in 1:gNIV)
          {
               cat(gVarNamesNormal[i],":",signif(t(a)[,i],gSIGDIG),"\n",sep="\t")
          }
     }
     
     cN <- F
     
     if(r==0) 
     {
          cN <- T
                    
          # creating and writing out the Log file header
          
          sink(paste(modelname,".log",sep=""))
          cat("Model Name:",modelname,"\n",sep="\t")
          cat("Number of individuals:",gNP,"\n",sep="\t")
          cat("Number of observations:",gNOBS,"\n",sep="\t")
          cat("Number of preliminary iterations:",gNCREP,"\n",sep="\t")
          cat("Number of draws used per individual:",gNEREP,"\n",sep="\t")
          cat("Random Seed:",gSeed,"\n",sep="\t")
          cat("Total iterations:",gNCREP + gNEREP,"\n",sep="\t")
          if(gNIV>0)
          {
               cat("Prior Variance:", priorVariance,"\n",sep="\t")
               cat("Degrees of Freedom:", degreesOfFreedom,"\n",sep="\t")
          }
          cat("Number of parameters:",gNIV + gFIV,"\n",sep="\t")
          
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
          if(!is.null(constraintsNorm))
          {
               cat("Constraints applied to random parameters (param1 - inequality - param2):","\n")
               for(i in 1:length(constraintsNorm))
               {
                    if(constraintsNorm[[i]][3]==0)
                         cat(gVarNamesNormal[constraintsNorm[[i]][1]],constraintLabels[constraintsNorm[[i]][2]],0,"\n")
                    if(constraintsNorm[[i]][3]!=0)
                         cat(gVarNamesNormal[constraintsNorm[[i]][1]],constraintLabels[constraintsNorm[[i]][2]],gVarNamesNormal[constraintsNorm[[i]][3]],"\n")
               }
               
          }
          
          cat("Estimated:",format(Sys.time(), "%a %b %d %X %Y"),"\n",sep="\t")
          
          cat("-----------------------------------------------------------","\n")
          
          cat("\n")
          if(gNIV > 0)
          {
               cat("Iteration","Log-Likelihood","RLH","Parameter RMS","Avg. Variance","\t")
          }
          if(gNIV == 0)
          {
               cat("Iteration","Log-Likelihood","RLH","\t")
          }          
          sink()	
          
          # setting up the plot for the alphas
          xmax <- gNCREP + gNEREP*gNSKIP+5000
          plot(r,0,main="Markov Chains",xlim=c(0,xmax),ylim=c(-5,5),pch=20,xlab="Iterations",ylab="Utility",axes=F)
          segments(gNCREP,-100,gNCREP,100,col="red",lty=2,lwd=2)
          segments(0,0,gNCREP + gNEREP*gNSKIP,0,col="gray",lty=1,lwd=1)
          axis(1,at=0:floor((gNCREP + gNEREP*gNSKIP)/5000)*5000)
          axis(2,at=-5:5)
          
     }	
     
     # plotting of the normals  to the screen
     if(gNIV >0)
     {
          for(i in 1:gNIV)
          {
               points(r,t(a)[i],pch=20,col=cr[i],cex=0.5)
          }
     }

     if(gFIV>0)
     {
          for(i in 1:gFIV)
          {
               points(r,fc[i],pch=20,col=cr[gNIV + i],cex=0.5)
          }
     }
     
     Sys.sleep(0) # forces replotting
     
     # Sink iteration details to the log file.
     sink(paste(modelname,".log",sep=""),append=T)
     
     cat("\n")
     
     if(gNIV > 0)
     {    
          cat(
               r,
               signif(sum(log(p)),gSIGDIG),
               signif(mean(p^(1/TIMES)),gSIGDIG),
               signif(paramRMS,gSIGDIG),
               signif(avgVariance,gSIGDIG),sep="\t"
               )
     }
     if(gNIV == 0)
     {
          
          cat(
               r,
               signif(sum(log(p)),gSIGDIG),
               signif(mean(p^(1/TIMES)),gSIGDIG),
               sep="\t"
          )
     }
     
     
     sink()
     
     # outputs to the screen time estimate of completion
     if(r>0)
     {
          cat("-----------------------------------------------------------","\n")
          
          tpi <- as.numeric(difftime(Sys.time(),starttime,units="sec"))/r
          cat("Time per iteration:",signif(tpi,1),"seconds")
          
          cat("\n")
          cat("Time to completion:",signif((gNCREP + gNEREP * gNSKIP - r)*tpi/60,4),"minutes")
          cat("\n")
     }
     cat("-----------------------------------------------------------","\n")
     
}
