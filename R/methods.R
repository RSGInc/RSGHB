
plot.RSGHB <- function(x, ...) { # add column argument?
     
     # Store old graphical parameters for later
     old.par <- par(no.readonly = TRUE)
     
     if (is.null(as.list(match.call())$type)) {
          type <- "Log"
     } else {
          type <- as.list(match.call())$type
     }
     
     # Plot means
     if (type == "A" | type == "F") {
          A <- x[[type]]
          
          if (is.null(A)) stop(paste0("model object does not contain component ", type))
          
          p <- ncol(A) - 1
          
          # Arrange plots in a roughly square grid
          par(oma = c(0, 0, 2, 0)) # can the margins be tightened further?
          if (p < 4) {
               par(mfrow = c(p, 1))
          } else {
               r <- ceiling(sqrt(p))
               if (r * (r - 1) >= p) {c <- r - 1} else {c <- r}               
               par(mfrow = c(r, c))
          }
          
          # Plot
          for (i in 1:p) plot(x = A[, 1], y = A[, 1 + i], type = "l", xlab = "Iteration", ylab = "Estimate", main = colnames(A)[1 + i])
          mtext("Markov Chains", outer = TRUE, cex = 1.5)          
          
#      } else if (type == "B") {
#           
#      } else if (type == "C") {
          
     } else if (type == "Log") {
          
          logStats <- x[["iter.detail"]]
          
          # Get valid columns
          stats <- names(logStats)[-1]
          valid.stats <- c()
          for (stat in stats) {
               if (!is.null(logStats[, stat]) & !all(is.na(logStats[, stat]))) valid.stats <- c(valid.stats, stat)
          }
          
          # Plot
          par(mfrow = c(length(valid.stats), 1), mar = c(4.1, 4.1, 2.1, 2.1))
          for (stat in valid.stats) {
               plot(x = logStats[, "Iteration"], logStats[, stat], type = "l", xlab = "Iteration", ylab = stat)
          }
     } else {
          
          stop("Invalid 'type' argument")
          
     }
     
     # Restore old graphical parameters
     par(old.par)
     
}


print.RSGHB <- function(model) {
     cat("Model:", model[["modelname"]])
     cat("\n\n")
     cat("Individuals:", length(unique(model[["C"]][, "Respondent"])))
     cat("\n")
     cat("Iterations Kept:", nrow(model[["A"]]))
     cat("\n\n")
     
     # summary fit statistics
     # report statistics for posterior iterations
     cat("Fit statistics\n")
     posterior <- (model[["iter.detail"]]$Iteration > model[["gNCREP"]])
     
     # need to make this clear if it is the lower level model
     if(!is.null(model[["A"]]))
     {
          cat("Mean log-likelihood (upper-level):",mean(model$sLikelihood),"\n")     
     }
     
     cat("Mean log-likelihood (lower-level):", mean(model[["iter.detail"]][posterior,"Log-Likelihood"]),"\n")
     cat("Mean root likelihood:", mean(model[["iter.detail"]][posterior,"RLH"]),"\n")
     cat("Hit rate:","***NEEDS Predict method**** hit rate table\n")
     cat("\n\n")
     
     cat("Model comparisons statistics\n")
     cat("Bayes Factor:",mean(model$sLikelihood)/model[["ll0"]],"\n") # can be used in calculating bayes factor
     
     D_hat <- -2*mean(model$sLikelihood)
     
     p_d   <- D_hat # - D(mean)
     
     DIC   <- p_d + D_hat
     
     cat("Deviance Information Criterion (not complete):",DIC,"\n")
     cat("\n")
         
     # if has random parameters
     if(!is.null(model[["A"]]))
     {
          posterior.means <- colMeans(model[["A"]][,-1])
          posterior.stdev <- apply(model[["A"]][,-1],2,sd)
     
          cat("Random Parameters (Underlying Normals)\n")
          cat("---------------------------------------------\n")
          cat("                        95% Credible Regions \n")
          print(
               data.frame(
                        Estimate  = signif(posterior.means, 3),
                        `Std Dev` = signif(posterior.stdev, 3),
                        `Min`     = apply(model[["A"]][,-1],2,quantile,0.025),
                        `Max`     = apply(model[["A"]][,-1],2,quantile,0.975),
                        check.names = FALSE,
                        row.names = model[["params.vary"]]
                        )
                )
     }
     
     # if has fixed parameters
     if(!is.null(model[["F"]]))
     {
          posterior.means <- colMeans(model[["F"]][,-1])
          posterior.stdev <- apply(model[["F"]][,-1],2,sd)
     
          cat("Fixed Parameters\n")
          cat("---------------------------------------------\n")
          cat("                        95% Credible Regions \n")
          print(
               data.frame(
                        Estimate  = signif(posterior.means, 3),
                        `Std Dev` = signif(posterior.stdev, 3),
                        `Min`     = apply(model[["F"]][,-1],2,quantile,0.025),
                        `Max`     = apply(model[["F"]][,-1],2,quantile,0.975),
                        check.names = FALSE,
                        row.names = model[["params.fixed"]]
                        )
                )
     }     

}
 


summary.RSGHB <- function(model) {
      print(model)
}
