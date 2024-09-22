Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
TimeMatrix <- Matrix[, 1, drop = FALSE]
Y <- Matrix[, 2, drop = FALSE]
X1 <- matrix(1, nrow = nrow(Y), ncol = 1)
X2 <- Matrix[, 3, drop = FALSE]
X3 <- Matrix[, 4, drop = FALSE]
X4 <- Matrix[, 5, drop = FALSE]
X5 <- Matrix[, 6, drop = FALSE]
X6 <- Matrix[, 7, drop = FALSE]
X7 <- Matrix[, 8, drop = FALSE]
X <- cbind(X1, X2, X3, X4, X5, X6, X7)

# ------------------------ Problem 3.(a) ------------------------

Beta_X <- function(X){
  return(solve(t(X) %*% X) %*% (t(X) %*% Y))
}

print(Beta_X(X))

# ------------------------ Problem 3.(b) ------------------------

X_1 <- cbind(X1, X2, X3)
X_2 <- cbind(X4, X5, X6, X7)
X1Y <- t(X_1) %*% Y
X2Y <- t(X_2) %*% Y
M2 <- rbind(X1Y, X2Y)
M11 <- t(X_1) %*% X_1
M12 <- t(X_1) %*% X_2
M21 <- t(X_2) %*% X_1
M22 <- t(X_2) %*% X_2
M1 <- solve(cbind(rbind(M11, M21), rbind(M12, M22)))
Beta <- M1 %*% M2

print(Beta)

# ------------------------ Problem 4. ------------------------

Y_average <- mean(Y)
TSS <- 0
for(i in 1:504){
  TSS <- TSS + (Y[i] - Y_average)^2
}
print(TSS)
  
Y_hat <- function(j, X){
  return(t(X[j]) %*% Beta[j])
}  

X1i <- matrix(1, nrow = nrow(Y), ncol = 1)
X2i <- cbind(X1i, Matrix[, 3, drop = FALSE])
X3i <- cbind(X2i, Matrix[, 4, drop = FALSE])
X4i <- cbind(X3i, Matrix[, 5, drop = FALSE])
X5i <- cbind(X4i, Matrix[, 6, drop = FALSE])
X6i <- cbind(X5i, Matrix[, 7, drop = FALSE])
X7i <- cbind(X6i, Matrix[, 8, drop = FALSE])
XList <- list(X1i, X2i, X3i, X4i, X5i, X6i, X7i)

for(j in 1:7){
  RSS <- 0
  Y_hat <- XList[[j]] %*% Beta_X(XList[[j]])
  for(i in 1:504){
    RSS <- RSS + ((Y_hat[i]) - Y_average)^2
  }
  print(paste("R-squared for X(i, ", j, ") = ", RSS/TSS, sep = ""))
}

  
  
  
  
  
  