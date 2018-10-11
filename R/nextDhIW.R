### CMC: new version with hierarchical IW
nextDhIW = function(a, b, env, lambda) {
  
  S = (b - t(matrix(a,nrow=env$gNIV,ncol=env$gNP)))
  
  Vnew = t(S)%*%S + 2*env$degreesOfFreedom*lambda
  
  Sigmanew = riwish(env$degreesOfFreedom+env$gNP+env$gNIV-1,Vnew)
  
  return(Sigmanew)
}
