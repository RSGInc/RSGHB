nextD <- function(a,b,env)
{		
     b <- matrix(t(a),nrow=env$gNP,ncol=env$gNIV,byrow=T) - b
     S <- t(b)%*%b + env$pvMatrix #env$priorVariance*diag(env$gNIV)	      # this is just the sample variance multiplied by N added with p * I
     # the left-hand side of the matrix mult is the choleski of S^-1 and the RH is the standard normal deviates
     R <- t(t(chol(solve(S)))%*%matrix(rnorm(env$gNIV*(env$gNP+env$gNIV+env$degreesOfFreedom)),nrow=env$gNIV,ncol=env$gNP+env$gNIV+env$degreesOfFreedom))
     R <- t(R)%*%R	
     return(solve(R))
}
