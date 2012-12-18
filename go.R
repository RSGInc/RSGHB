go <-
function()
{          
     if(checkModel())
     {
          r <- 1
          
          ma <- matrix(0,nrow=gNIV,ncol=gNEREP)
	  md <- matrix(0,nrow=gNIV*(gNIV+1)/2,ncol=gNEREP)
	  mb <- matrix(0,nrow=gNP,ncol=gNIV)
	  mb.squared <- matrix(0,nrow=gNP,ncol=gNIV)
	  mp <- matrix(0,nrow=gNP,ncol=gNEREP)
	  mf <- matrix(0,nrow=gFIV,ncol=gNEREP)
	  mc <- matrix(0,nrow=gNP,ncol=gNIV)
     	  mc.squared <- matrix(0,nrow=gNP,ncol=gNIV)	 # variance calculation
          
          
          out <- hb(A, B, Dmat, FC, ma, md, mb, mb.squared, mp, mf, mc, mc.squared)
     
          MA   <- data.frame(iteration=1:gNEREP/gNSKIP,t(out[[1]]))
          MD   <- data.frame(iteration=1:gNEREP/gNSKIP,t(out[[2]]))
          MC   <- data.frame(id=respIDs,RLH=out[[4]],out[[3]])
          if(gFIV>0)
          {   
               MF  <- data.frame(iteration=1:gNEREP/gNSKIP,t(out[[5]]))
               names(MF) <- c("iteration",gVarNamesFixed)
          }          
          MCSd <- data.frame(id=respIDs,sqrt(out[[6]]))
          storedDraws <- out[[7]]          
          MB   <- data.frame(id=respIDs,out[[8]])
          MBSd <- data.frame(id=respIDs,sqrt(out[[9]]))
          
     
          names(MC) <- c("Respondent","RLH",gVarNamesNormal)
          names(MA) <- c("iteration",gVarNamesNormal)
          
          names(MCSd) <- c("Respondent",gVarNamesNormal)
          names(MB)   <- c("Respondent",gVarNamesNormal)
          names(MBSd) <- c("Respondent",gVarNamesNormal)
          
          cat("Creating output files. Please be patient.","\n")
          
	  dev.copy(png,paste(modelname,"_markovChains.png",sep=""))
	  dev.off()
     
          write.table(MA,paste(modelname,"_A.csv",sep=""),sep=",",row.names=F)
          write.table(MD,paste(modelname,"_D.csv",sep=""),sep=",",row.names=F)
       	  write.table(MB,paste(modelname,"_B.csv",sep=""),sep=",",row.names=F)
	  write.table(MBSd,paste(modelname,"_Bsd.csv",sep=""),sep=",",row.names=F)
          write.table(MC,paste(modelname,"_C.csv",sep=""),sep=",",row.names=F)
          write.table(MCSd,paste(modelname,"_Csd.csv",sep=""),sep=",",row.names=F)

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
