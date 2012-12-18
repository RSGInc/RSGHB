vech <-
function(mat)
{
     
     nr <- dim(mat)[1]
     nc <- dim(mat)[2]
     
     rV <- rep(0,nr*(nc+1)/2)
     
     k <- 1
     
     for(i in 1:nr)
     {
          for(j in 1:i)
          {
               if(j<=i){
                    rV[k] = mat[i,j]
                    k <- k + 1
               }
          }
     }
     return(rV)
}
