hb = function(a, b, d, f, env = parent.frame()) {
  
  env$starttime <- Sys.time()
  
  if (env$gSeed == 0) {
    env$gSeed <- ceiling(runif(1) * 1e+06)
  }
  set.seed(env$gSeed, kind = "default", normal.kind = "default")
    
  ### CMC: set lambda 
  if(env$hIW){
    lambda = diag(env$gNIV)
    for(l in 1:env$gNIV){
      lambda[l,l] = 1/rinvgamma(1,shape=(1/2),scale=(1/(env$xi^2)))
    }
  }
  
  p <- env$likelihood(f, b, env)
    
  for (r in 1:env$gNCREP) {
    if (env$gNIV > 0) {
      out <- nextB(a, b, d, p, f, env)
      b <- out[[1]]
      env$acceptanceRatePerc <- out[[2]]
      p <- out[[3]]
      a <- nextA(b, d, env)
      if (!is.null(env$fixedA)) {
        for (rp in 1:env$gNIV) {
          if (!is.na(env$fixedA[rp])) {
            a[rp] <- env$fixedA[rp]
          }
        }
      }
      if (env$gFULLCV){ 
      ### CMC: different functions used depending on whether hIW is true
	   if(env$hIW){
          d <- nextDhIW(a, b, env,lambda)
          lambda <- nextlambda(d,env)
        } else {
          d <- nextD(a, b, env)  
        }
      }
      if (!env$gFULLCV) {
        d <- nextDind(a, b, env)
      }
      if (!is.null(env$fixedD)) {
        for (rp in 1:env$gNIV) {
          if (!is.na(env$fixedD[rp])) {
            d[rp, rp] <- env$fixedD[rp]
          }
        }
      }
    }
    if (env$gFIV > 0) {
        out <- nextF(p, f, b, env)
        if (sum(out[[1]] == f) != env$gFIV) {
          env$acceptanceRateF <- env$acceptanceRateF + 1
        }
        if ((r%%100) == 0) {
          env$acceptanceRateFPerc <- env$acceptanceRateF/100
          if (env$acceptanceRateFPerc < env$targetAcceptanceFixed) {
            env$rhoF <- env$rhoF - env$rhoF/50
          }
          if (env$acceptanceRateFPerc > env$targetAcceptanceFixed) {
            env$rhoF <- env$rhoF + env$rhoF/50
          }
          env$acceptanceRateF <- 0
        }
        f <- out[[1]]
        p <- out[[2]]
      }
    if (r%%env$gINFOSKIP == 0 | r == 1) {
        progreport(r, p, a, b, d, f, env)
      }
    if (env$gStoreDraws) {
      for (i in 1:env$gNP) {
        env$storedDraws[[i]] <- matrix(0, env$gNEREP, env$gNIV)
        dimnames(env$storedDraws[[i]]) <- list(NULL, env$gVarNamesNormal)
      }
    }
  }
  #browser()
  n <- env$gNEREP * env$gNSKIP
  for (r in 1:n) {
    if (env$gNIV > 0) {
      out <- nextB(a, b, d, p, f, env)
      b <- out[[1]]
      env$acceptanceRatePerc <- out[[2]]
      p <- out[[3]]
      a <- nextA(b, d, env)
      if (!is.null(env$fixedA)) {
        for (rp in 1:env$gNIV) {
          if (!is.na(env$fixedA[rp])) {
            a[rp] <- env$fixedA[rp]
          }
        }
      }
      if (env$gFULLCV){ 
        ### CMC: different functions used depending on whether hIW is true
        if(env$hIW){
          d <- nextDhIW(a, b, env,lambda)
          lambda <- nextlambda(d,env)
        } else {
          d <- nextD(a, b, env)  
        }
      }
      if (!env$gFULLCV) {
        d <- nextDind(a, b, env)
      }
      if (!is.null(env$fixedD)) {
        for (rp in 1:env$gNIV) {
          if (!is.na(env$fixedD[rp])) {
            d[rp, rp] <- env$fixedD[rp]
          }
        }
      }
      if (r%%env$gNSKIP == 0) {
        C <- trans(b, env)
        env$ma[, r/env$gNSKIP] <- a
        env$md[, , r/env$gNSKIP] <- d
        #env$mp[, r/env$gNSKIP] <- p^(1/env$TIMES)
        env$mb <- env$mb + b
        env$mb.squared <- env$mb.squared + b^2
        env$mc <- env$mc + C
        env$mc.squared <- env$mc.squared + C^2
      
        if (env$gStoreDraws) {
          for (i in 1:env$gNP) {
            env$storedDraws[[i]][(r/env$gNSKIP), ] <- C[i, ]
          }
        }
      }
    }
    if (env$gFIV > 0) {
      
      out <- nextF(p, f, b, env)
      if (sum(out[[1]] == f) != env$gFIV) {
        env$acceptanceRateF <- env$acceptanceRateF + 1
        if ((r%%100) == 0) {
          env$acceptanceRateFPerc <- env$acceptanceRateF/100
          if (env$acceptanceRateFPerc < env$targetAcceptanceFixed) {
            env$rhoF <- env$rhoF - env$rhoF/50
          }
          if (env$acceptanceRateFPerc > env$targetAcceptanceFixed) {
            env$rhoF <- env$rhoF + env$rhoF/50
          }
          env$acceptanceRateF <- 0
        }
      }
      f <- out[[1]]
      p <- out[[2]]

      if (r %% env$gNSKIP == 0) {
        env$mf[, (r/env$gNSKIP)] <- f
      }
    }
    ### CMC: independently of whether we've used random or fixed, we now save LL and RLH, next four lines added
    if (r %% env$gNSKIP == 0) {
      env$cmcLLout[[(r / env$gNSKIP)]] <- p
      #env$cmcRLHout[, (r/env$gNSKIP)] <- p^(1/env$TIMES)
    }
    if (r %% env$gINFOSKIP == 0) {
      progreport(env$gNCREP + r, p, a, b, d, f, env)
    }
  }
  env$cmcLLout = do.call(cbind, env$cmcLLout)
  return(TRUE)
}
