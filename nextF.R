nextF <- function(p,f,b)
{
     
     fnew <- f + sqrt(rhoF) * rnorm(gFIV,0,1)
     
     pnew <- likelihood(fnew,b)
     
     r    <- prod(pnew/p)
     
     ind  <- ( runif(1) <= r )
          
     pnew <- pnew * ind + p * ( 1 - ind )
     
     fnew <- as.vector(t(fnew %*% t(ind)) + t(f %*% t( 1 - ind )))

     return(list(fnew,pnew))
}
