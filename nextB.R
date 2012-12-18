nextB <-
function(a, b, d, p, f)
{
     
     # note the multiplication by rho. this is the "jumping distribution"
     bnew <- b + matrix(rnorm(gNP*gNIV),nrow=gNP,ncol=gNIV)%*%chol(d)*sqrt(rho)
     
     bn   <- bnew - matrix(t(a),nrow=gNP,ncol=gNIV,byrow=T)
     bo   <- b - matrix(t(a),nrow=gNP,ncol=gNIV,byrow=T)
     
     pnew <- likelihood(f,bnew)

     # gives the relative density (or the probability of seeing the betas) given current estimates
     # of D and A	
     # the relative densities are important because without any reference to how the betas fit in with the current
     # estimates of A and D, you'd end up with 
     r.new <- pnew / p * exp(-0.5 * (colSums(t(bn)*(solve(d)%*%t(bn))) -	colSums(t(bo)*(solve(d)%*%t(bo)))))
     
     # if r.new > 1 then we accept the new estimate of beta. if r.new < 1 then we accept the new estimate
     # with probability = r.new
     
     ind  <- (r.new >= 1) + (r.new < 1)*(matrix(runif(gNP),nrow=gNP) <= r.new)
     nind <- 1*(ind==0)
     
     # this is the acceptance rate. the target for this 0.3 (though Sawtooth allows for the user to specify this).
     i <- colSums(ind)/gNP

     if(i < 0.3)
     {
          rho <<- rho - 0.001
     }
     if(i > 0.3)
     {
          rho <<- rho + 0.001
     }

     if(rho<0)
     {
          rho <<-0.001          
     }
     
     # i've just converted it to matrix form to make the multiplication simpler.
     mind  <- matrix(0,nrow = gNP,ncol=gNIV)
     mnind <- matrix(0,nrow = gNP,ncol=gNIV)
     
     mind[,1:gNIV]  <- ind
     mnind[,1:gNIV] <- 1*(ind == 0)
     
     return(list(mind * bnew + mnind * b, i, ind * pnew + nind * p))
     
}
