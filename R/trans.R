trans <- function(b)
{
     C <- b
     
     for(k in 1:gNIV)
     {
          
          if(gDIST[k] ==2)	     # positive log-normal
          {
               C[,k] <- exp(b[,k])
          }
          if(gDIST[k] ==3)	     # negative log-normal
          {
               C[,k] <- -1*exp(b[,k])
          }    
          if(gDIST[k] ==4)         # postive truncated normal
          {
               C[,k] <- b[,k]*(b[,k] >= 0)
          }
          if(gDIST[k] == 5)        # Johnson SB
          {	
               C[,k] <- exp(b[,k]) / (1 + exp(b[,k]))
               C[,k] <- (C[,k] * ( gMAXCOEF[k] - gMINCOEF[k] ) ) + gMINCOEF[k]    	
          }              
          
          k <- k + 1
          
     }
     
     return(C)
}
