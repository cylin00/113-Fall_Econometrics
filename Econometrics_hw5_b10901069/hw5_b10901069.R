library(sandwich) 

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
  new_names[1] <- "constant"  
  new_names <- sub("^Xx_", "x_", new_names) 
  new_names <- sub("^X", "", new_names) 
  names(coef) <- new_names
  
  return(coef)
}

# ------------------------ Problem 1 ------------------------

model <- lm(Y ~ X-1)
beta <- model$coefficients
print(UpdateName(beta))

s <- summary(model)$sigma

S_beta <- s * sqrt(diag(solve(t(X) %*% X))) 
print(UpdateName(S_beta))

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
  t_statistic = t_statistic,                             
  alpha_1_significant = t_statistic > alpha_1,           # Significance under alpha = 1%
  alpha_5_significant = t_statistic > alpha_5,           # Significance under alpha = 5%
  alpha_10_significant = t_statistic > alpha_10          # Significance under alpha = 10%
)

print(result_df)

# ------------------------ Problem 3 ------------------------

# 3 - (a)

variance_hat <- sum(resid(model)^2) / length(resid(model))
std_hat <- sqrt(variance_hat)
sk_hat <- sum((resid(model) / std_hat)^3) / length(resid(model))
kr_hat <- sum((resid(model) / std_hat)^4) / length(resid(model))

JB <- length(resid(model)) * ((sk_hat^2 / 6) + ((kr_hat - 3)^2 / 24))
print(JB)

chi_1 <- qchisq(1 - 0.01, 2)  # alpha = 1%
chi_5 <- qchisq(1 - 0.05, 2)  # alpha = 5%
chi_10 <- qchisq(1 - 0.10, 2)  # alpha = 10%

print(paste("Significant at 1% level:", JB > chi_1))
print(paste("Significant at 5% level:", JB > chi_5))
print(paste("Significant at 10% level:", JB > chi_10))

# 3 - (b)

standardized_residuals <- resid(model) / std_hat
density_res <- density(standardized_residuals)

plot(density_res, main = "Kernel Density vs N(0,1)", xlim = c(-6, 6))

curve(dnorm(x), col = "red", add = TRUE)

legend("topright", legend = c("Kernel Density", "N(0,1)"), col = c("black", "red"), lty = 1)






