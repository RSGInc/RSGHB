hb <-
function(a, b, d, f)
{
    
     starttime <<- Sys.time()
     
     # sets random seed.	  
     if(gSeed==0)
     {
     	  gSeed <<- ceiling(runif(1)*1000000)
     }
     	       
     set.seed(gSeed,kind="default",normal.kind="default")
     
     p <- likelihood(f,b)
     
     # Iterate until convergence #
     
     writeLog(0,p,a,b,f)
     
     # the burn-in iterations
     for(r in  1:gNCREP)
     {
          out    <- nextB(a, b, d, p, f)
          b      <- out[[1]]
          accept <- out[[2]]
          p      <- out[[3]]
          
          a <- nextA(b, d)

          # drawing a new set of fixed coefficients
          if(gFIV > 0)
          {
               out <- nextF(p,f,b)
               
               f  <- out[[1]]
               p  <- out[[2]]     
          }
               
          if(gFULLCV==1) {
               d <- nextD(a, b)
          } 
          if(gFULLCV==0) {
               d <- nextDind(a, b)
          }
          
          if(r%%gINFOSKIP == 0) {
               writeLog(r,p,a,b,f)
          }
          
          r <- r + 1
     }
     
     # Post Burn-in iterations          
     ma <- matrix(0,nrow=gNIV,ncol=gNEREP)
     md <- matrix(0,nrow=gNIV*(gNIV+1)/2,ncol=gNEREP)
     mb <- matrix(0,nrow=gNP,ncol=gNIV)
     mb.squared <- matrix(0,nrow=gNP,ncol=gNIV)
     mp <- matrix(0,nrow=gNP,ncol=gNEREP)
     mf <- matrix(0,nrow=gFIV,ncol=gNEREP)
     mc <- matrix(0,nrow=gNP,ncol=gNIV)
     mc.squared <- matrix(0,nrow=gNP,ncol=gNIV)	 # variance calculation
     
     storedDraws <- list()
     
     # for storing draws
     if(gStoreDraws)
     {          
          for(i in 1:gNP)
          {
               storedDraws[[i]] <- matrix(0,gNEREP,gNIV)
               dimnames(storedDraws[[i]])<-list(NULL,gVarNamesNormal)
          }    
     }
     
     
     # Iterate after convergence collecting averages 
     n <- gNEREP * gNSKIP
     r <- 1
     
     for(r in 1:n)
     {
          out    <- nextB(a, b, d, p, f)
          b      <- out[[1]]
          accept <- out[[2]]
          p      <- out[[3]]
          
          a <- nextA(b, d)
          
          if(gFIV > 0)
          {
               # drawing a new set of fixed coefficients
               out <- nextF(p,f,b)
               f   <- out[[1]]
               p    <- out[[2]]
               mf[ ,(r/gNSKIP)] <- f
          }
                    
          if(gFULLCV==1) {
               d <- nextD(a, b)
          }	
          if(gFULLCV==0) {
               d <- nextDind(a, b)
          }
          
          if(r%%gNSKIP == 0) {
               C <- trans(b)
               ma[ ,(r/gNSKIP)] <- a
               md[ ,(r/gNSKIP)] <- vech(d)
               mp[,r/gNSKIP] <- p^(1/TIMES)    #RLH Calculation
               mb <- mb + b
               mb.squared <- mb.squared + b^2
               mc <- mc + C
               mc.squared <- mc.squared + C^2
               if(gStoreDraws)
               {
                 for(i in 1:gNP)
                 {
                   storedDraws[[i]][(r/gNSKIP),] <- C[i,]
                 }
                    
               }
          }
          
          if(r%%gINFOSKIP == 0) {
               
               writeLog(gNCREP + r,p,a,b,f)
               
          }
     }
     
     return(list(ma,md,mc/gNEREP,rowMeans(mp),mf,(mc.squared-mc^2/gNEREP)/gNEREP,storedDraws,mb/gNEREP,(mb.squared-mb^2/gNEREP)/gNEREP))
}
