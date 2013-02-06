plotLog <- function(model)
{
     
     #create the filename string
     fn <- paste(model,".log",sep="")
     
     i <- 0 
     
     # finde the starting spot of the statistics
     lineChar <- ""
     while(lineChar != "Iteration")
     {
          lineChar <- read.table(fn,nrows=1,header=F,skip=i)[1]
          i <- i + 1
     }
     
     logStats <- read.table(fn,header=F,skip=i+1,sep="\t")
     
     hasRandom <- F
     
     if(dim(logStats)[2]==5)
     {
          hasRandom <- T     
     }
        
     names(logStats)[1:3] <- c("Iteration","Log-Likelihood","RLH")
     # need to adjust this if we ever add more statistics to the plotting
     if(hasRandom) 
     {
          names(logStats)[4:5] <- c("Param_RMS","Avg. Variance")
     }
     # adjust the par settings to allow for stacked plotting
     
     fn.png <- paste(model,"_logPlot.png",sep="")
     
     # plot once to a file
     png(fn.png)
     
     par(mfrow=c(2+hasRandom*2,1),mar=c(4.1,4.1,2.1,2.1))
     
     # plot each of the statistics to assese convergence
     plot(logStats[,1],logStats[,2],type="l",xlab=names(logStats)[1],ylab=names(logStats)[2])
     plot(logStats[,1],logStats[,3],type="l",xlab=names(logStats)[1],ylab=names(logStats)[3])
     
     if(hasRandom)
     {
          plot(logStats[,1],logStats[,4],type="l",xlab=names(logStats)[1],ylab=names(logStats)[4])
          plot(logStats[,1],logStats[,5],type="l",xlab=names(logStats)[1],ylab=names(logStats)[5])
     }
     
     dev.off()
     
     par(mfrow=c(2+hasRandom*2,1),mar=c(4.1,4.1,2.1,2.1))
     
     # plot once to the screen     
     plot(logStats[,1],logStats[,2],type="l",xlab=names(logStats)[1],ylab=names(logStats)[2])
     plot(logStats[,1],logStats[,3],type="l",xlab=names(logStats)[1],ylab=names(logStats)[3])
     
     if(hasRandom)
     {
          plot(logStats[,1],logStats[,4],type="l",xlab=names(logStats)[1],ylab=names(logStats)[4])
          plot(logStats[,1],logStats[,5],type="l",xlab=names(logStats)[1],ylab=names(logStats)[5])
     }
     
     # reset the par values so we don't effect other plotting
     par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
     
}