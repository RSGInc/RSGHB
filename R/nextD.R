nextD <-
function(a,b)
{		
     b <- matrix(t(a),nrow=gNP,ncol=gNIV,byrow=T) - b
     S <- t(b)%*%b + priorVariance*diag(gNIV)	      # this is just the sample variance multiplied by N added with p * I
     # the left-hand side of the matrix mult is the choleski of S^-1 and the RH is the standard normal deviates
     R <- t(t(chol(solve(S)))%*%matrix(rnorm(gNIV*(gNP+gNIV+degreesOfFreedom)),nrow=gNIV,ncol=gNP+gNIV+degreesOfFreedom))
     R <- t(R)%*%R	
     return(solve(R))
}
