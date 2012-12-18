nextA <-
function(b,d)
{
     SQNP <- sqrt(gNP)
     
     # draws from multivariate normals can be taken using 
     # draws = mu + l * eta where l*l' = d (l is the cholesky decomposition)      
     
     #accounting for non-diffuse priors
     
     return(colMeans(b) + t(chol(d))%*%matrix(rnorm(gNIV),nrow=gNIV,ncol=1)/SQNP)
}
