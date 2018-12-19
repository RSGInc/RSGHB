doHB = function(likelihood_user, choicedata, control = list()) {
  likelihood <- function(fc, b, env) {
    if (env$gNIV > 0) {
      gIDS <- env$gIDS
      C <- trans(b, env)
      if (env$gNIV > 1) 
        C <- C[gIDS, ]
      if (env$gNIV == 1) 
        C <- matrix(C[gIDS], ncol = env$gNIV)
    }
    p <- likelihood_user(fc, C)
    p <- replace(p, is.na(p), 9.88131291682493e-324)
    p0 <- rep(1, env$gNP)
    p0 <- .C("aggregation", as.double(env$gIDS), as.double(env$gNOBS), 
            as.double(env$gNP), as.double(p), as.double(1:env$gNP), 
            as.double(p0))[[6]]
    p0 <- replace(p0, p0 < 9.88131291682493e-324, 9.88131291682493e-324)
    return(p0)
  }
  if (is.null(control[["modelname"]])) {
    modelname <- "HBModel"
  } else {
    modelname <- control[["modelname"]]
  }
  if (is.null(control[["gVarNamesNormal"]])) {
    gVarNamesNormal <- c()
  } else {
    gVarNamesNormal <- control[["gVarNamesNormal"]]
  }
  if (is.null(control[["gVarNamesFixed"]])) {
    gVarNamesFixed <- c()
  } else {
    gVarNamesFixed <- control[["gVarNamesFixed"]]
  }
  if (is.null(control[["gDIST"]])) {
    gDIST <- rep(1, length(gVarNamesNormal))
  } else {
    gDIST <- control[["gDIST"]]
  }
  if (is.null(control[["FC"]])) {
    FC <- rep(0, length(gVarNamesFixed))
  } else {
    FC <- control[["FC"]]
  }
  if (is.null(control[["svN"]])) {
    svN <- rep(0, length(gVarNamesNormal))
  } else {
    svN <- control[["svN"]]
  }
  if (is.null(control[["gNCREP"]])) {
    gNCREP <- 1e+05
  } else {
    gNCREP <- control[["gNCREP"]]
  }
  if (is.null(control[["gNEREP"]])) {
    gNEREP <- 1e+05
  } else {
    gNEREP <- control[["gNEREP"]]
  }
  if (is.null(control[["gNSKIP"]])) {
    gNSKIP <- 1
  } else {
    gNSKIP <- control[["gNSKIP"]]
  }
  if (is.null(control[["gINFOSKIP"]])) {
    gINFOSKIP <- 250
  } else {
    gINFOSKIP <- control[["gINFOSKIP"]]
  }
  if (is.null(control[["constraintsNorm"]])) {
    constraintsNorm <- NULL
  } else {
    constraintsNorm <- control[["constraintsNorm"]]
  }
  if (is.null(control[["fixedA"]])) {
    fixedA <- NULL
  } else {
    fixedA <- control[["fixedA"]]
  }
  if (is.null(control[["fixedD"]])) {
    fixedD <- NULL
  } else {
    fixedD <- control[["fixedD"]]
  }
  if (is.null(control[["nodiagnostics"]])) {
    nodiagnostics <- FALSE
  } else {
    nodiagnostics <- control[["nodiagnostics"]]
  }
  if (is.null(control[["gSIGDIG"]])) {
    gSIGDIG <- 10
  } else {
    gSIGDIG <- control[["gSIGDIG"]]
  }
  if (is.null(control[["priorVariance"]])) {
    priorVariance <- 2
  } else {
    priorVariance <- control[["priorVariance"]]
  }
  if (is.null(control[["degreesOfFreedom"]])) {
    degreesOfFreedom = 5
  } else {
    degreesOfFreedom <- control[["degreesOfFreedom"]]
  }
  ### CMC: additional checks for using hIW
  if (is.null(control[["hIW"]])|is.null(control[["gVarNamesNormal"]])) {
    hIW <- FALSE
  } else {
    hIW <- control[["hIW"]]
  }
  if (is.null(control[["xi"]]) & hIW == TRUE) {
    xi <- 1
  } else {
    xi <- control[["xi"]]
  }
  ### CMC: additional check to ensure > 1 when using hIW
  if (degreesOfFreedom < 2 & hIW == TRUE){
    degreesOfFreedom=2
    cat("Degrees of freedom set to 2 - lowest possible value")
  }
  if (is.null(control[["rho"]])) {
    rho <- 0.1
  } else {
    rho <- control[["rho"]]
  }
  if (is.null(control[["rhoF"]])) {
    rhoF <- 1e-04
  } else {
    rhoF <- control[["rhoF"]]
  }
  if (is.null(control[["gFULLCV"]])) {
    gFULLCV <- TRUE
  } else {
    gFULLCV <- control[["gFULLCV"]]
  }
  if (is.null(control[["gStoreDraws"]])) {
    gStoreDraws <- FALSE
  } else {
    gStoreDraws <- control[["gStoreDraws"]]
  }
  if (is.null(control[["gSeed"]])) {
    gSeed <- 0
  } else {
    gSeed <- control[["gSeed"]]
  }
  if (is.null(control[["gMINCOEF"]])) {
    gMINCOEF <- 0
  } else {
    gMINCOEF <- control[["gMINCOEF"]]
  }
  if (is.null(control[["gMAXCOEF"]])) {
    gMAXCOEF <- 0
  } else {
    gMAXCOEF <- control[["gMAXCOEF"]]
  }
  if (is.null(control[["pvMatrix"]])) {
    useCustomPVMatrix <- FALSE
    pvMatrix <- NULL
  } else {
    useCustomPVMatrix <- TRUE
    pvMatrix <- control[["pvMatrix"]]
  }
  if (is.null(control[["targetAcceptanceNormal"]])) {
    targetAcceptanceNormal <- 0.3
  } else {
    targetAcceptanceNormal <- control[["targetAcceptanceNormal"]]
  }
  if (is.null(control[["targetAcceptanceFixed"]])) {
    targetAcceptanceFixed <- 0.3
  } else {
    targetAcceptanceFixed <- control[["targetAcceptanceFixed"]]
  }
  if (is.null(control[["writeModel"]])) {
    writeModel <- FALSE
  } else {
    writeModel <- control[["writeModel"]]
  }
  if (is.null(control[["verbose"]])) {
    verbose <- TRUE
  } else {
    verbose <- control[["verbose"]]
  }
  
  gNP <- 0
  gNOBS <- 0
  TIMES <- 0
  gIDS <- 0
  respIDs <- 0
  A <- 0
  B <- 0
  Dmat <- 0
  Choice <- 0
  gNIV <- length(gVarNamesNormal)
  gFIV <- length(gVarNamesFixed)
  if (is.null(pvMatrix) & gNIV > 0) {
    pvMatrix <- priorVariance * diag(gNIV)
  }
  if (!is.matrix(pvMatrix) & gNIV > 0) {
    stop("\npvMatrix is not a matrix. Make sure that your prior covariance matrix is ", gNIV, " by ", gNIV, ".")
  }
  if (!is.null(pvMatrix) & gNIV > 0) {
    if (nrow(pvMatrix) != gNIV | ncol(pvMatrix) != gNIV) {
      stop("\nThe prior covariance matrix is of the wrong size. Make sure that your prior covariance matrix is ", gNIV, " by ", gNIV, ".")
    }
  }
  rownames(pvMatrix) <- colnames(pvMatrix) <- gVarNamesNormal
  
  begintime <- Sys.time()
  starttime <- Sys.time()
  distNames <- c("N", "LN+", "LN-", "CN+", "CN-", "JSB")
  constraintLabels <- c("<", ">")
  acceptanceRatePerc <- 0
  acceptanceRateF <- 0
  acceptanceRateFPerc <- 0
  rhoFadj <- 1e-05
  if (checkModel(nodiagnostics = nodiagnostics, verbose = verbose)) {
    r <- 1
    ma <- matrix(0, nrow = gNIV, ncol = gNEREP)
    md <- array(0, dim = c(gNIV, gNIV, gNEREP), dimnames = list(NULL, NULL, 1:gNEREP))
    mb <- matrix(0, nrow = gNP, ncol = gNIV)
    mb.squared <- matrix(0, nrow = gNP, ncol = gNIV)
    mp <- matrix(0, nrow = gNP, ncol = gNEREP)
    mf <- matrix(0, nrow = gFIV, ncol = gNEREP)
    mc <- matrix(0, nrow = gNP, ncol = gNIV)
    mc.squared <- matrix(0, nrow = gNP, ncol = gNIV)
    ### CMC: added two output matrices in two lines below
    cmcLLout  <- matrix(0, nrow = gNP, ncol = gNEREP)
    cmcRLHout <- matrix(0, nrow = gNP, ncol = gNEREP)
    storedDraws <- list()
    results <- list(modelname = modelname, params.fixed = gVarNamesFixed, 
                    params.vary = gVarNamesNormal, distributions = distNames[gDIST], 
                    pv = pvMatrix, df = degreesOfFreedom, gSIGDIG = gSIGDIG, 
                    gNP = gNP, gNOBS = gNOBS, gNCREP = gNCREP, gNEREP = gNEREP, 
                    gSeed = gSeed, constraints = constraintsNorm, iter.detail = data.frame(Iteration = NA, 
                    `Log-Likelihood` = NA, RLH = NA, `Parameter RMS` = NA, 
                    `Avg. Variance` = NA, `Acceptance Rate (Fixed)` = NA, 
                    `Acceptance Rate (Normal)` = NA, check.names = FALSE),
                    ### CMC: included new outputs in the results list in two lines below
                    cmcLLout = cmcLLout,
                    cmcRLHout = cmcRLHout)
    hb(A, B, Dmat, FC)
    matTIMES = matrix(1/TIMES,nrow=gNP,ncol=gNEREP,byrow = FALSE)
    if (gNIV > 0) {
      #browser()
      mp = cmcLLout^(matTIMES)
      ma <- cbind(iteration = (gNCREP + 1):(gNCREP + gNEREP), t(ma))
      mcsd <- cbind(id = respIDs, sqrt((mc.squared - mc^2/gNEREP)/gNEREP))
      mc <- cbind(id = respIDs, RLH = rowMeans(mp), mc/gNEREP)
      mbsd <- cbind(id = respIDs, sqrt((mb.squared - mb^2/gNEREP)/gNEREP))
      mb <- cbind(id = respIDs, mb/gNEREP)
      colnames(mc) <- c("Respondent", "RLH", gVarNamesNormal)
      colnames(ma) <- c("iteration", gVarNamesNormal)
      colnames(mcsd) <- c("Respondent", gVarNamesNormal)
      colnames(mb) <- c("Respondent", gVarNamesNormal)
      colnames(mbsd) <- c("Respondent", gVarNamesNormal)
      results$A <- ma
      results$B <- mb
      results$Bsd <- mbsd
      results$C <- mc
      results$Csd <- mcsd
      results$D <- md
    }
    if (gFIV > 0) {
      mf <- cbind(iteration = (gNCREP + 1):(gNCREP + gNEREP), t(mf))
      colnames(mf) <- c("iteration", gVarNamesFixed)
      results$F <- mf
    }
    ### CMC: independently of whether we've used random or fixed, we now save LL and RLH, next two lines added
    results$cmcLLout <- cmcLLout
    results$cmcRLHout <- results$cmcLLout^(matTIMES)
    if (gStoreDraws) {
      results$Draws <- storedDraws
      names(results$Draws) <- respIDs
    }
    results$choices <- Choice
    results$p <- likelihood_user(
      fc = if (is.null(results[["F"]])) {
        NULL
      } else {
        colMeans(as.matrix(results[["F"]][, -1]))
      }, 
      b = if (is.null(results[["C"]])) {
        NULL
      } else {
        as.matrix(results[["C"]][gIDS, -c(1:2)])
      })
    results$ll0 <- sum(log(likelihood_user(fc = FC, b = matrix(svN, ncol = length(svN), nrow = length(gIDS), byrow = TRUE))))
    results$llf <- sum(log(results[["p"]]))
    results[["iter.detail"]] <- results[["iter.detail"]][-1,]
    if (verbose) {
      cat("Estimation complete.\n")
    }
      class(results) <- "RSGHB"
    if (writeModel) {
      if (verbose) {
        cat("Creating output files. Please be patient.\n")
      }
      writeModel(results)
      if (verbose) {
        cat("Output files finished writing to working directory.\n")
      }
    }
  } else {
    results <- NULL
  }
  return(results)
}