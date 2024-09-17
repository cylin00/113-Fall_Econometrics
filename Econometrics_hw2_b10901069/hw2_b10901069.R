Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
TimeMatrix <- Matrix[, 1, drop = FALSE]
Y <- Matrix[, 2, drop = FALSE]
X1 <- Matrix[, 3, drop = FALSE]
X2 <- Matrix[, 6, drop = FALSE]
X3 <- Matrix[, 9, drop = FALSE]

# ----------------------- Problem 4 -----------------------

# - - - - - - - - - - - - Sub-Problem 1- - - - - - - - - - - -

SA_Y <- mean(Y)
indices1 <- X1 > 0.015
indices2 <- X1 <= 0.015
Y_a1 <- Y[indices1, , drop = FALSE]
Y_a2 <- Y[indices2, drop = FALSE]
SA_Y1 <- mean(Y_a1)
SA_Y2 <- mean(Y_a2)

a1 <- (SA_Y - SA_Y2) / (SA_Y1 - SA_Y2)
a2 <- 1 - a1
print(a1)
print(a2)

# - - - - - - - - - - - - Sub-Problem 2 - - - - - - - - - - - -

indices11 <- X1 > 0.015 & X2 > 0.02
indices12 <- X1 > 0.015 & X2 <= 0.02
indices21 <- X1 <= 0.015 & X2 > 0.02
indices22 <- X1 <= 0.015 & X2 <= 0.02
Y_b11 <- Y[indices11, , drop = FALSE]
Y_b12 <- Y[indices12, , drop = FALSE]
Y_b21 <- Y[indices21, , drop = FALSE]
Y_b22 <- Y[indices22, , drop = FALSE]
b11 <- length(Y_b11) / length(Y)
b12 <- length(Y_b12) / length(Y)
b21 <- length(Y_b21) / length(Y)
b22 <- length(Y_b22) / length(Y)

print(b11)
print(b12)
print(b21)
print(b22)

# - - - - - - - - - - - - Sub-Problem 3 - - - - - - - - - - - -

indices31 <- X1 > 0.015 & X3 > -4
indices32 <- X1 <= 0.015 & X3 > -4
indices3 <- X3 > -4
Y_g <- Y[indices3, , drop = FALSE]
Y_g1 <- Y[indices31, , drop = FALSE]
Y_g2 <- Y[indices32, , drop = FALSE]
g1 <- length(Y_g1) / length(Y_g)
g2 <- length(Y_g2) / length(Y_g)

print(g1)
print(g2)

# - - - - - - - - - - - - 3 Decimal - - - - - - - - - - - -

print(round(a1, 3))
print(round(a2, 3))
print(round(b11, 3))
print(round(b12, 3))
print(round(b21, 3))
print(round(b22, 3))
print(round(g1, 3))
print(round(g2, 3))



