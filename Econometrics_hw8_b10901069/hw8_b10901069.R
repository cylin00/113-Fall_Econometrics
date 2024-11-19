# ------------------------------------------------

Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
Y <- Matrix[, 2, drop = FALSE]
x1 <- matrix(1, nrow = nrow(Y), ncol = 1)
x_dfy <- Matrix[, 3, drop = FALSE]
x_infl <- Matrix[, 4, drop = FALSE]
x_svar <- Matrix[, 5, drop = FALSE]
x_tms <- Matrix[, 6, drop = FALSE]
x_tbl <- Matrix[, 7, drop = FALSE]
x_dfy2 <- x_dfy ** 2
x_infl2 <- x_infl ** 2
x_svar2 <- x_svar ** 2
x_tms2 <- x_tms ** 2
x_tbl2 <- x_tbl ** 2
X <- cbind(x1, x_dfy, x_infl, x_svar, x_tms, x_tbl, x_dfy2, x_infl2, x_svar2, x_tms2, x_tbl2)
colnames(X) <- c("ones", "x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl",
                 "x_dfy2", "x_infl2", "x_svar2", "x_tms2", "x_tbl2")

# ------------------------------------------------

n <- nrow(Y)
k <- ncol(X)
model_indices <- expand.grid(rep(list(c(FALSE, TRUE)), k))
num_models <- nrow(model_indices)

# Storage for model selection criteria
criteria <- data.frame(CenteredR2 = numeric(num_models),
                       AdjustedR2 = numeric(num_models),
                       AIC = numeric(num_models),
                       BIC = numeric(num_models),
                       Cp = numeric(num_models),
                       LOOCV = numeric(num_models))


for (i in 1:num_models) {
  
  if (sum(model_indices[i, ]) == 0) {
    next  # Skip this iteration if no predictors are selected
  }
  
  # Convert row of model_indices to logical vector for subsetting
  predictors <- X[, which(as.logical(model_indices[i, ])), drop = FALSE]
  
  # Fit the model
  model <- lm(Y ~ predictors - 1)  # Fit model without intercept (centered)
  
  # Calculate metrics
  criteria$CenteredR2[i] <- summary(model)$r.squared
  criteria$AdjustedR2[i] <- summary(model)$adj.r.squared
  criteria$AIC[i] <- AIC(model)
  criteria$BIC[i] <- BIC(model)
  criteria$Cp[i] <- sum(residuals(model)^2) / var(residuals(model))  # Simplified Mallows' Cp
  criteria$LOOCV[i] <- mean((residuals(model) / (1 - lm.influence(model)$hat))^2)  # LOO-CV
}

# Identify best models
best_models <- list(
  CenteredR2 = which.max(criteria$CenteredR2),
  AdjustedR2 = which.max(criteria$AdjustedR2),
  AIC = which.min(criteria$AIC),
  BIC = which.min(criteria$BIC),
  Cp = which.min(criteria$Cp),
  LOOCV = which.min(criteria$LOOCV)
)


pdf("plot.pdf", width = 7, height = 5)
plot(criteria$CenteredR2, main = "Centered R2", ylab = "R2", cex = 0.5, pch = 16)
plot(criteria$AdjustedR2, main = "Adjusted R2", ylab = "Adj R2", cex = 0.5, pch = 16)
plot(criteria$AIC, main = "AIC", ylab = "AIC", cex = 0.5, pch = 16)
plot(criteria$BIC, main = "BIC", ylab = "BIC", cex = 0.5, pch = 16)
plot(criteria$Cp, main = "Mallows' Cp", ylab = "Cp", cex = 0.5, pch = 16)
plot(criteria$LOOCV, main = "LOO-CV", ylab = "LOO Error", cex = 0.5, pch = 16)
dev.off()  

print(best_models)
