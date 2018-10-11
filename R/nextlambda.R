### CMC: next lambda function 
nextlambda = function(d,env){
  
  lambdanew = diag(env$gNIV)
  
  invSigma = chol2inv(chol(d))
  
  for(j in 1:env$gNIV){
    lambdanew[j,j] = 1/rinvgamma(1,shape=((env$degreesOfFreedom+env$gNIV)/2),scale=((1/(env$xi^2))+(env$degreesOfFreedom*invSigma[j,j])))
  }
  
  return(lambdanew)
}
