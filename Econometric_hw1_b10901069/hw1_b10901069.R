Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
TimeMatrix <- Matrix[, 1, drop = FALSE]
YMatrix <- Matrix[, 2, drop = FALSE]
XMatrix <- Matrix[, -c(1, 2), drop = FALSE]

#-------------------- Problem 1 --------------------

TimeSerie <- function(X, yLab){
  return(plot(TimeMatrix, X , type = "l", col = rgb(0, 0, 1), xlab = "Time", ylab = yLab, main = "Time Serie of X's column"))
}

#X's Time Series Plot
for(i in 1:ncol(XMatrix)) 
{
  png(paste(colnames(XMatrix)[i], "_TimeSeriesPlot.png", sep = ""))
  TimeSeriesPlot(XMatrix[, i], colnames(XMatrix)[i])
  dev.off()
}

#X's Histogram
png(paste("X_Histogram.png"))
hist(XMatrix, probability = TRUE, main = "Histogram of X", xlab = "Values", col = rgb(0.1, 0.3, 0.4, 0.5), border = "white", breaks = 20)
lines(density(rnorm(5544)), col = rgb(0.7, 0.1, 0.1, 0.7), lwd = 1.5)
dev.off()

#Y's Time Series Plot
png(paste(colnames(YMatrix), "_TimeSeriesPlot.png", sep = ""))
TimeSeriesPlot(YMatrix, colnames(YMatrix))
dev.off()

#Y's Histogram
png(paste("Y_Histogram.png"))
hist(YMatrix, probability = TRUE, main = "Histogram of Y", xlab = "Values", col = rgb(0.1, 0.3, 0.4, 0.5), border = "white", breaks = 20)
lines(density(rnorm(504)), col = rgb(0.7, 0.1, 0.1, 0.7), lwd = 1.5)
dev.off()

#-------------------- Problem 2 --------------------

#Problem 2-1
Matrix_2_1 <- XMatrix %*% solve(t(XMatrix) %*% XMatrix) %*% t(XMatrix)
TraceOfMatrix_2_1 <- sum(diag(Matrix_2_1))
print(TraceOfMatrix_2_1)

#Problem 2-2
I_n <- diag(504)
Matrix_2_2 <- I_n - Matrix_2_1
TraceOfMatrix_2_2 <- sum(diag(Matrix_2_2))
print(TraceOfMatrix_2_2)

#-------------------- Problem 3 --------------------

eigenX <- eigen(t(XMatrix) %*% XMatrix)
print(eigenX$values)
png(paste("ScreePlot.png"))
plot(1:11, eigenX$values, type = "l", col = rgb(0, 0, 1), xlab = "j", ylab = "eigenvalue", main = "Scree Plot")
dev.off()






