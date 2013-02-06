hb <- function(a, b, d, f, env=parent.frame())
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
     
     writeLog(0,p,a,b,d,f)
     
     # the burn-in iterations
     for(r in  1:gNCREP)
     {

          if(gNIV > 0) {
          
               out    <- nextB(a, b, d, p, f)
               b      <- out[[1]]
               accept <- out[[2]]
               p      <- out[[3]]
          
               a <- nextA(b, d)
               
               if(gFULLCV==1) {
                    d <- nextD(a, b)
               } 
               if(gFULLCV==0) {
                    d <- nextDind(a, b)
               }
          }

          # drawing a new set of fixed coefficients
          if(gFIV > 0) {
               out <- nextF(p,f,b)
               
               f  <- out[[1]]
               p  <- out[[2]]     
          }
                   
          if(r%%gINFOSKIP == 0) {
               writeLog(r,p,a,b,d,f)
          }
          
          r <- r + 1
     }
     
     # for storing draws
     if(gStoreDraws){     
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
          
          if(gNIV >0){
                    
               out    <- nextB(a, b, d, p, f)
               b      <- out[[1]]
               accept <- out[[2]]
               p      <- out[[3]]     
               
               a <- nextA(b, d)
               
               if(gFULLCV==1) {
                    d <- nextD(a, b)
               }     
               if(gFULLCV==0) {
                    d <- nextDind(a, b)
               }
               
               if(r%%gNSKIP == 0) {
                    C <- trans(b)
                    env$ma[ ,(r/gNSKIP)] <- a
                    env$md[ ,(r/gNSKIP)] <- vech(d)
                    env$mp[,r/gNSKIP] <- p^(1/TIMES)    #RLH Calculation
                    env$mb <- env$mb + b
                    env$mb.squared <- env$mb.squared + b^2
                    env$mc <- env$mc + C
                    env$mc.squared <- env$mc.squared + C^2
                    if(gStoreDraws)
                    {
                         for(i in 1:gNP)
                         {
                              env$storedDraws[[i]][(r/gNSKIP),] <- C[i,]
                         }
                         
                    }
               } 
               
          }
          
          if(gFIV > 0){
               # drawing a new set of fixed coefficients
               out <- nextF(p,f,b)
               f   <- out[[1]]
               p   <- out[[2]]
               
               if(r%%gNSKIP == 0) {
                    env$mf[ ,(r/gNSKIP)] <- f
               }
               
          }              
          
          if(r%%gINFOSKIP == 0) {
               
               writeLog(gNCREP + r,p,a,b,d,f)
               
          }
     }
     
     return(TRUE)
}
