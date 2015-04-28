progreport <- function(r, p, a, b, d, f, env)
{

     if (env$gNIV > 0)
     {
          paramRMS <- sqrt(mean(apply(trans(b,env), 1, function(x) x^2)))   
          avgVariance <- mean(apply(trans(b, env), 1, var))
     }
     else
     {
          paramRMS <- NA
          avgVariance <- NA
     }
     
     # Some printing progress to the screen
     if (env$verbose) {
          cat(rep("\n", 128))
          cat("-----------------------------------------------------------","\n")
          cat("Iteration: ", r, "\n",sep="\t")
          cat("-----------------------------------------------------------","\n")
          # Model Statistics
          if(env$gNIV > 0) {
               cat("RHO (Normal): ", env$rho, "\n")
               cat("Acceptance Rate (Normal): ", env$acceptanceRatePerc, "\n")  
          }
          if(env$gFIV > 0) {
               cat("RHO (Fixed): ", env$rhoF, "\n")     
               cat("Acceptance Rate (Fixed): ", env$acceptanceRateFPerc, "\n")     
          }
          cat("Log-Likelihood: ", signif(sum(log(p)), env$gSIGDIG), "\n", sep = "\t")
          cat("RLH: ", signif(mean(p^(1/env$TIMES)), env$gSIGDIG), "\n", sep = "\t")
          if(env$gNIV > 0)
          {
               cat("Parameter RMS:", signif(paramRMS, env$gSIGDIG), "\n")
               cat("Avg. Variance:", signif(avgVariance, env$gSIGDIG), "\n")
          }
          
          cat("-----------------------------------------------------------","\n")
          
          # fixed coefficients
          if(env$gFIV > 0)
          {
               cat("Current values for fixed coefficients","\n")
               for(i in 1:env$gFIV) cat(env$gVarNamesFixed[i], ":", signif(f[i], env$gSIGDIG), "\n", sep = "\t")
          }
          
          # Normal Coefficients
          if(env$gNIV > 0)
          {
               cat("Current values for the population means of the underlying normals","\n")
               for(i in 1:env$gNIV) cat(env$gVarNamesNormal[i],":",signif(t(a)[,i], env$gSIGDIG),"\n",sep="\t")
          }
          
          # outputs to the screen time estimate of completion
          if(r > 0)
          {
               cat("-----------------------------------------------------------","\n")
               
               tpi <- (Sys.time() - env$starttime)/env$gINFOSKIP
               env$starttime <- Sys.time() # makes the forecast based on the most recent iterations
               tleft <- (env$gNCREP + env$gNEREP * env$gNSKIP - r)*tpi
               units(tleft) <- "mins"
               
               cat("Time per iteration:", format(tpi, digits = 3))
               cat("\n")
               cat("Time to completion:", format(tleft, digits = 3))
               cat("\n")
          }
          cat("-----------------------------------------------------------","\n")
          
          # plotting of the normals  to the screen
          if (env$gNIV > 0 & env$gFIV > 0) {
               
               alphas <- c(r, a, f)
               
          } else if (env$gNIV > 0) {
               
               alphas <- c(r, a)
               
          } else if (env$gFIV > 0) {
               
               alphas <- c(r, f)
               
          }
          
          cr <- rainbow(length(alphas) - 1)
          
          # initial plot
          if (r == 1) {
               xmax <- (env$gNCREP + env$gNEREP * env$gNSKIP) * 1.05
               plot(x = 0, y = 0, main = "Markov Chains", xlim = c(0, xmax), ylim = c(-5, 5), pch = 20, xlab = "Iterations", ylab = "Utility", axes = FALSE, col = "white", cex = 0.5)
               segments(env$gNCREP, -100, env$gNCREP, 100, col = "red", lty = 2, lwd = 2)
               segments(0, 0, env$gNCREP + env$gNEREP * env$gNSKIP, 0, col = "gray", lty = 1, lwd = 1)
               axis(1, at = seq(from = 0, to = env$gNCREP + env$gNEREP * env$gNSKIP, by = floor((env$gNCREP + env$gNEREP * env$gNSKIP) / 10)))
               axis(2, at = -100:100)
          }
          
     
          # Plotting Alphas
          for (i in 2:length(alphas)) points(x = alphas[1], y = alphas[i], pch = 20, col = cr[i - 1], cex = 0.5)
          
          Sys.sleep(0) # forces replotting
     }
     
     # Sink iteration details to the log file.
     detail <- c(r,
                 signif(sum(log(p)), env$gSIGDIG),
                 signif(mean(p^(1/env$TIMES)), env$gSIGDIG),
                 signif(paramRMS, env$gSIGDIG),
                 signif(avgVariance, env$gSIGDIG),
                 signif(env$acceptanceRateFPerc, env$gSIGDIG),
                 signif(env$acceptanceRatePerc, env$gSIGDIG))
     
     env$results[["iter.detail"]] <- rbind(env$results[["iter.detail"]], detail)
     
}
