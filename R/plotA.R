plotA <- function(object, columns = NULL)
{
     
     A <- data.frame(object[["A"]])
     
     gNIV <- ncol(A) - 1
     numIts <- nrow(A)
     graphics.off()
     par(ask = TRUE)
     
     if (is.null(columns)) columns <- 1:gNIV
     
     for (i in columns)
     {
          par(oma = c(3, 0, 0, 0))
          
          x <- (1:numIts)
          trend <- lm(A[, i + 1] ~ x)
          
          plot(A[, i + 1], type = "l", main = names(A)[i + 1], xlab = "Iteration", ylab = "Value")
          lines(1:numIts, trend$fitted.values, col = "Red", lwd = 2)
          
          coefficients <- signif(summary(trend)$coefficients, 2)
          model.out1 <- paste("Slope: ", coefficients[2, 1], "      ", sep = " ")
          model.out2 <- paste("T-Test (0): ", coefficients[2, 3], "      ", sep = " ")
          model.out3 <- paste("R^2: ", signif(summary(trend)$r.squared, 3), sep = "")

          out <- paste(model.out1, model.out2, model.out3, sep = "")
          mtext("Trend line regression", side = 1, outer = TRUE, font = 2, line = 0, padj = 0)
          mtext(out, side = 1, outer = TRUE, font = 1, line = 1, padj = 0)
          dev.flush()
          
     }     
     par(oma = c(0, 0, 0, 0))
     par(ask = FALSE)
}