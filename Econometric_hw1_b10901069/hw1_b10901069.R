Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)

TimeMatrix <- Matrix[, 1, drop = FALSE]
YMatrix <- Matrix[, 2, drop = FALSE]
XMatrix <- Matrix[, -c(1, 2), drop = FALSE]

print(XMatrix[, 1]) 

#----------Problem-1----------

#X's Time Series Plot
TimeSerie <- function(X, yLab){
  return(plot(TimeMatrix, X , type = "l", col = rgb(0, 0, 1), xlab = "Time", ylab = yLab, main = "Time Serie of X's column"))
}

for(i in 1:ncol(XMatrix)) 
{
  png(paste(colnames(XMatrix)[i], "_TimeSeriesPlot.png", sep = ""))
  TimeSeriesPlot(XMatrix[, i], colnames(XMatrix)[i])
  dev.off()
}

#X's Histogram
estimatedData <- rnorm(5544)
Histogram <- function(X){
  return(hist(X, probability = TRUE, main = "Histogram of X", xlab = "Values", col = rgb(0.1, 0.3, 0.4, 0.5), border = "white", breaks = 20))
}

png(paste("X_Histogram.png"))
Histogram(XMatrix)
density_estimate <- density(estimatedData)
lines(density_estimate, col = rgb(0.7, 0.1, 0.1, 0.7), lwd = 1.5)
dev.off()


