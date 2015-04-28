print.RSGHB <- function(model, conf.level = 0.95) {
     
     alpha <- (1 - conf.level)/2
     
     A.mean <- colMeans(as.matrix(model[["A"]][, -1]))
     A.sd   <- apply(X = as.matrix(model[["A"]][, -1]), MARGIN = 2, FUN = sd)
     A.t    <- A.mean / A.sd
     A.p    <- pnorm(abs(A.t), lower.tail = FALSE)
     sig    <- ifelse(A.p < 0.001, "***",
                      ifelse(A.p < 0.01, "**",
                             ifelse(A.p < 0.05, "*",
                                    ifelse(A.p < 0.1, ".", " "))))
     
     cat("Model:", model[["modelname"]])
     cat("\n")
     cat("Estimated in", format(model[["duration"]], format = "%h:%Mm:%s", digits = 3), "on", format(model[["endtime"]], "%a %b %d %X %Y"))
     cat("\n\n")
     cat("Individuals:", length(unique(model[["C"]][, "Respondent"])))
     cat("\n")
     cat("Iterations Kept:", nrow(model[["A"]]))
     cat("\n\n")
     cat("Hyper-Parameter Estimates (Underlying Normals)\n")
     cat("---------------------------------------------\n")
     print(data.frame(Estimate = signif(A.mean, 3),
                      `Std Dev` = signif(A.sd, 3),
                      t = signif(A.t, 3),
                      `P(>|t|)` = signif(A.p, 3),
                      ` ` = sig,
                      check.names = FALSE,
                      row.names = model[["params.vary"]]))
     cat("--------------------------------------------------------------\n")
     cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
     cat("--------------------------------------------------------------\n")
     cat(round(conf.level*100, 0), "%", " level of confidence\n\n", sep = "")
     
     if (!is.null(model[["choices"]])) {
          cat("Prediction rates\n")
          cat("----------------\n")
          pred.table <- aggregate(model[["p"]], by = list(model[["choices"]]), FUN = mean)
          names(pred.table) <- c("Choice", "Prob.")
          pred.table[,2] <- paste0(round(pred.table[,2] * 100, 1), "%")
          print(pred.table)
     }
}

summary.RSGHB <- function(model) {
     print(model)
}
# 
# Here are some thoughts:
#   1.  Model fit
# 2.	Prediction table
# 3.	The statistics should reflect the Bayesian-ness of the model - for example, std error = std dev. 
# 4.	Use the Bayesian equivalent of p-values and t-tests.
# 5.	I would like to see the bayes factor relative to a null model.

