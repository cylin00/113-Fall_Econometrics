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

UpdateName <- function(coef){
  new_names <- names(coef)
  new_names[1] <- "constant"  # Set the first name to "constant"
  new_names <- sub("^Xx_", "x_", new_names)  # Replace "Xx_" with "x_"
  new_names <- sub("^X", "", new_names)  # Remove leading "X" for other variables
  names(coef) <- new_names
  
  return(coef)
}

# ------------------------ Problem 1 ------------------------

model <- lm(Y ~ X-1)
beta <- model$coefficients
print(beta)

s <- summary(model)$sigma

S_beta <- s * sqrt(diag(solve(t(X) %*% X))) 
print(UpdateName(S_beta))

library(sandwich)
Vw <- vcovHC(model, type = "HC")
SW_beta <- sqrt(diag(Vw))
print(UpdateName(SW_beta))

# ------------------------ Problem 2 ------------------------

betaJ <- summary(model)$coefficients[, "Estimate"]
sBetaJ <- summary(model)$coefficients[, "Std. Error"]
t_statistic <- abs(betaJ / sBetaJ)

df <- length(model$residuals) - length(t_statistic) # Degrees of freedom
alpha_1 <- qt(1 - 0.01 / 2, df)  # Two-tailed test, alpha = 1%
alpha_5 <- qt(1 - 0.05 / 2, df)  # Two-tailed test, alpha = 5%
alpha_10 <- qt(1 - 0.10 / 2, df)  # Two-tailed test, alpha = 10%


result_df <- data.frame(
  t_statistic = t_statistic,                             # Original t-statistics
  alpha_1_significant = t_statistic > alpha_1,           # Significance under alpha = 1%
  alpha_5_significant = t_statistic > alpha_5,           # Significance under alpha = 5%
  alpha_10_significant = t_statistic > alpha_10          # Significance under alpha = 10%
)

# Print the result
print(result_df)

