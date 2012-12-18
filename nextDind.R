nextDind <-
function(a, b)
{
     d <- matrix(0, gNIV, gNIV)
     b <- matrix(t(a),nrow=gNP,ncol=gNIV,byrow=T) - b
     
     for(k in 1:gNIV)
     {
          t <- 1 + t(b[,k])%*%b[,k]
          s <- sqrt(1/t)[1,1]*matrix(rnorm(gNP+1),nrow=gNP+1,ncol=1)
          
          d[k,k] <- solve(t(s)%*%s)
     }
     
     return(d)
}
