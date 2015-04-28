plotF <- function(object, columns = NULL)
{
     
     if (is.null(object[["F"]])) stop("No fixed parameters to plot.")
  
     Fdata <- data.frame(object[["F"]])
     
     gFIV <- ncol(Fdata) - 1
     numIts <- nrow(Fdata)
     
     graphics.off()
     par(ask = TRUE)
     
     if (is.null(columns)) columns <- 1:gFIV
     
     for(i in columns)
     {
          par(oma = c(3, 0, 0, 0))
          x <- (1:numIts)
          trend <- lm(Fdata[, i + 1] ~ x)
          
          plot(Fdata[, i + 1], type = "l", main = names(Fdata)[i + 1], xlab = "Iteration", ylab = "Value")
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