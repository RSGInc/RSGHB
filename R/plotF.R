plotF <- function(model,columns=0)
{

     fn <- paste(model,"_F.csv",sep="")
     
     F <- read.table(fn,sep=",",header=T)
     
     gFIV <- ncol(F)-1
     numIts <- nrow(F)
     
     graphics.off()
     par(ask=T)
     
     if(columns==0)
     {
     	columns <- 1:gFIV
     }
     
     for(i in columns)
     {
          par(oma=c(3,0,0,0))
          x <- (1:numIts)
          trend <- lm(F[,i+1]~x)
          
          plot(F[,i+1],type="l",main=names(F)[i+1],xlab="Iteration",ylab="Value")
          lines(1:numIts,trend$fitted.values,col="Red",lwd=2)
          
          coefficients <- signif(summary(trend)$coefficients,2)          
          model.out1 <- paste("Slope: ",coefficients[2,1], "      ",sep=" ")
          model.out2 <- paste("T-Test (0): ",coefficients[2,3], "      ",sep=" ")
          model.out3 <- paste("R^2: ",signif(summary(trend)$r.squared,3),sep="")

          out <- paste(model.out1,model.out2,model.out3,sep="")
          mtext("Trend line regression",side=1,outer=T,font=2,line=0,padj=0)
          mtext(out,side=1,outer=T,font=1,line=1,padj=0)
          dev.flush()
          
     }    
     par(oma=c(0,0,0,0))
     par(ask=F)
}