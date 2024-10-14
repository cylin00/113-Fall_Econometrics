Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
Y <- Matrix[, 2, drop = FALSE]
X1 <- matrix(1, nrow = nrow(Y), ncol = 1)
X_dfy <- Matrix[, 3, drop = FALSE]
X_infl <- Matrix[, 4, drop = FALSE]
X_svar <- Matrix[, 5, drop = FALSE]
X_tms <- Matrix[, 6, drop = FALSE]
X_tbl <- Matrix[, 7, drop = FALSE]
X_dfr <- Matrix[, 8, drop = FALSE]
X_db <- Matrix[, 9, drop = FALSE]
X_ltr <- Matrix[, 10, drop = FALSE]
X <- cbind(X1, X_dfy, X_infl, X_svar, X_tms, X_tbl, X_dfr, X_db, X_ltr)

# ------------------------ Problem 1 ------------------------

model <- lm(Y ~ X-1)
beta <- model$coefficients
print(beta)

s <- summary(model)$sigma

S_beta <- s * sqrt(diag(solve(t(X) %*% X))) 
print(S_beta)

library(sandwich)
Vw <- vcovHC(model, type = "HC")
SW_beta <- sqrt(diag(Vw))
print(SW_beta)

# ------------------------ Problem 2 ------------------------












