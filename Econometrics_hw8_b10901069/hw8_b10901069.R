# ------------------------------------------------
rm(list = ls())

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
predictor_indices <- vector("list", length(num_models))

criteria <- data.frame(CenteredR2 = numeric(num_models),
                       AdjustedR2 = numeric(num_models),
                       AIC = numeric(num_models),
                       BIC = numeric(num_models),
                       Cp = numeric(num_models),
                       LOOCV = numeric(num_models))

num_predictors <- vector("list", length(num_models))

for (i in 2:num_models) {
  
  predictor_indices[[i]] <- which(as.logical(model_indices[i, ]))
  
  predictors <- X[, which(as.logical(model_indices[i, ])), drop = FALSE]
  
  num_predictors[i] <- sum(model_indices[i, ])
  
  model <- lm(Y ~ predictors - 1)

  criteria$CenteredR2[i] <- summary(model)$r.squared
  criteria$AdjustedR2[i] <- summary(model)$adj.r.squared
  criteria$AIC[i] <- AIC(model)
  criteria$BIC[i] <- BIC(model)
  criteria$Cp[i] <- sum(residuals(model)^2) / var(residuals(model))  # ******
  criteria$LOOCV[i] <- mean((residuals(model) / (1 - lm.influence(model)$hat))^2)  # ******
  
}

# Now we modify the best_models list to select the second minimum for Cp and LOOCV
best_models <- list(
  CenteredR2 = which.max(criteria$CenteredR2),
  AdjustedR2 = which.max(criteria$AdjustedR2),
  AIC = which.min(criteria$AIC),
  BIC = which.min(criteria$BIC),
  Cp = order(criteria$Cp),
  LOOCV = order(criteria$LOOCV)
)

best_models

p <- c(best_models$CenteredR2, best_models$AdjustedR2, best_models$AIC, best_models$BIC, best_models$Cp, best_models$LOOCV)
for (i in 1:length(p)) {
  print(paste("Model", i, "uses", num_predictors[p[i]], "predictors:", paste(colnames(X)[predictor_indices[[p[i]]]], collapse = ", ")))
}

