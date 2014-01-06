plotC <- function (model, columns = NULL) 
{
  fn <- paste(model, "_C.csv", sep = "")
  C <- read.table(fn, sep = ",", header = TRUE)
  gNIV <- ncol(C) - 2
  numIDs <- nrow(C)
  graphics.off()
  par(ask = TRUE)
  if (is.null(columns)) {
    columns <- 1:gNIV + 2
  }
  for (i in columns) {
    plot(density(C[, i]), type = "l", main = paste(names(C)[i], "\n",
                                                   sum(C[,i] >= 0), " (", round(sum(C[,i] >= 0) / length(C[,i]) * 100, 1),
                                                   "%) >= 0", sep = ""), 
         xlab = "Utility", ylab = "Density")
    dev.flush()
  }
  par(ask = F)
}
