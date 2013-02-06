go <- function(nodiagnostics=F)
{          
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
