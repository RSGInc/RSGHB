plotLog <- function(object) {
     
     # Grab the model statistics
     logStats <- object[["iter.detail"]]
     
     # How many valid columns are there?
     stats <- names(logStats)[-1]
     valid.stats <- c()
     for (stat in stats) {
          if (!is.null(logStats[, stat]) & !all(is.na(logStats[, stat]))) valid.stats <- c(valid.stats, stat)
     }
     
     # Store all original graphical values
     orig.par <- par(c("mfrow", "mar"))
     
     par(mfrow = c(length(valid.stats), 1), mar = c(4.1, 4.1, 2.1, 2.1))
     # plot each of the statistics to the screen
     for (stat in valid.stats) {
          plot(x = logStats[, "Iteration"], logStats[, stat], type = "l", xlab = "Iteration", ylab = stat)
     }
     
     # reset the par values so we don't effect other plotting
     par(orig.par)
}
