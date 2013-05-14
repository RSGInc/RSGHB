xpnd <-
function(vec)
{
     lv <- length(vec)
     
     nr <- (-1 + sqrt(8*lv+1))/2
     nc <- nr
     
     rM <- matrix(0,nrow=nr,ncol=nc)
     
     k <- 1
     for(i in 1:nr)
     {
          for(j in 1:i)
          {
               if(j<=i){
                    rM[i,j] <- vec[k]
                    k <- k + 1
               }
          }	
     }
     
     for(i in 1:nc)
     {
     	rM[i,] <- t(rM[,i])               
     }
     
     return(rM)
}
