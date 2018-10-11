prepareModel = function(env) {
  
  env$gNP <- length(unique(env$choicedata$ID))
  env$Choice <- env$choicedata$Choice
  env$gNOBS <- dim(env$choicedata)[1]
  env$TIMES <- matrix(0, nrow = env$gNP, ncol = 1)
  env$TIMES[, 1] <- aggregate(env$choicedata$ID, by = list(env$choicedata$ID), length)[, 2]
  env$gIDS <- unlist(as.vector(mapply(rep, 1:env$gNP, env$TIMES)))
  env$respIDs <- unique(env$choicedata$ID)
  
  if (length(env$gVarNamesNormal) > 0) {
    env$A <- matrix(0, nrow = env$gNIV, ncol = 1)
    env$B <- matrix(0, nrow = env$gNP, ncol = env$gNIV)
    env$Dmat <- env$priorVariance * diag(env$gNIV)
    env$A[, 1] <- env$svN
    env$B <- 1 + env$B
    env$B <- env$B * matrix(t(env$A), nrow = env$gNP, ncol = env$gNIV, byrow = T)
  }
}
