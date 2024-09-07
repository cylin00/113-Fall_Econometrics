Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
TimeMatrix <- Matrix[, 1, drop = FALSE]
YMatrix <- Matrix[, 2, drop = FALSE]
XMatrix <- Matrix[, -c(1, 2), drop = FALSE]

#-------------------- Problem 1 --------------------

TimeSerie <- function(X, yLab){
  return(plot(TimeMatrix, X , type = "l", col = rgb(0, 0, 1), xlab = "Time", ylab = yLab, main = "Time Serie of X's column"))
}

Histogram <- function(X, FigureName){
  hist(X, probability = TRUE, main = FigureName, xlab = "Values", col = rgb(0.1, 0.3, 0.4, 0.5), border = "white", breaks = 20)
}

#X's Time Series Plot
for(i in 1:ncol(XMatrix)) 
{
  png(paste(colnames(XMatrix)[i], "_TimeSeriesPlot.png", sep = ""))
  TimeSeriesPlot(XMatrix[, i], colnames(XMatrix)[i])
  dev.off()
}

#X's Histogram
for(i in 1:ncol(XMatrix)) 
{
  png(paste(colnames(XMatrix)[i], "_Histogram.png", sep = ""))
  Histogram(XMatrix[, i], colnames(XMatrix)[i])
  #lines(density(rnorm(504)), col = rgb(0.7, 0.1, 0.1, 0.7), lwd = 1.5)
  lines(density(XMatrix[, i],), col = rgb(0.7, 0.1, 0.1, 0.7), lwd = 1.5)
  dev.off()
}

#Y's Time Series Plot
png(paste(colnames(YMatrix), "_TimeSeriesPlot.png", sep = ""))
TimeSeriesPlot(YMatrix, colnames(YMatrix))
dev.off()

#Y's Histogram

png(paste("Y_Histogram.png"))
Histogram(YMatrix, colnames(YMatrix))
lines(density(YMatrix), col = rgb(0.7, 0.1, 0.1, 0.7), lwd = 1.5)
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
png(paste("ScreePlot_Problem3.png"))
plot(1:11, eigenX$values, type = "l", col = rgb(0, 0, 1), xlab = "j", ylab = "eigenvalue", main = "Scree Plot")
dev.off()

#-------------------- Problem 4 --------------------

Standardized_X <- scale(XMatrix)
eigen_4_1 <- eigen(t(Standardized_X) %*% Standardized_X)
eigen_4_2 <- eigen(Standardized_X %*% t(Standardized_X))

png(paste("ScreePlot_Problem4.png"), width = 800, height = 400)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(1, 1))
plot(1:11, eigen_4_1$values, type = "l", col = rgb(0, 0, 1), xlab = "j", ylab = "eigenvalue", main = "Scree Plot 1", pch = 16)
plot(1:504, eigen_4_2$values, type = "l", col = rgb(1, 0, 0), xlab = "j", ylab = "eigenvalue", main = "Scree Plot 2", pch = 16)
dev.off()

#-------------------- Problem 5 --------------------

MatrixA <- solve(t(Standardized_X) %*% Standardized_X)
EigenA <- eigen(MatrixA)
P <- EigenA$vectors
D <- diag(EigenA$values)
A_Reconstructed <- P %*% D %*% solve(P)

print(A_Reconstructed %*% solve(A_Reconstructed))





