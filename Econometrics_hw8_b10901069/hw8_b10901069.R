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
predictor_indices <- vector("list", length = num_models)

criteria <- data.frame(CenteredR2 = numeric(num_models),
                       AdjustedR2 = numeric(num_models),
                       AIC = numeric(num_models),
                       BIC = numeric(num_models),
                       Cp = numeric(num_models),
                       LOOCV = numeric(num_models))

num_predictors <- numeric(num_models)

full_model <- lm(Y ~ X - 1)
full_sigma2 <- sum(residuals(full_model)^2) / (n - k)

for (i in 1:num_models) {
  
  predictor_indices[[i]] <- which(as.logical(model_indices[i, ]))
  
  if (length(predictor_indices[[i]]) == 0) {
    num_predictors[i] <- 0
    next
  }
  
  predictors <- X[, predictor_indices[[i]], drop = FALSE]
  
  num_predictors[i] <- sum(model_indices[i, ])
  
  model <- lm(Y ~ predictors - 1)
  
  ESS <- sum(residuals(model)^2)
  H <- predictors %*% solve(t(predictors) %*% predictors) %*% t(predictors)
  h_ii <- diag(H)
  
  criteria$CenteredR2[i] <- summary(model)$r.squared
  criteria$AdjustedR2[i] <- summary(model)$adj.r.squared
  criteria$AIC[i] <- AIC(model)
  criteria$BIC[i] <- BIC(model)
  criteria$Cp[i] <- ESS / full_sigma2 + 2 * num_predictors[i] - n 
  criteria$LOOCV[i] <- mean((residuals(model) / (1 - h_ii))^2) 
}

valid_indices <- which(num_predictors > 0)

best_models <- list(
  CenteredR2 = valid_indices[which.max(criteria$CenteredR2[valid_indices])],
  AdjustedR2 = valid_indices[which.max(criteria$AdjustedR2[valid_indices])],
  AIC = valid_indices[which.min(criteria$AIC[valid_indices])],
  BIC = valid_indices[which.min(criteria$BIC[valid_indices])],
  Cp = valid_indices[which.min(criteria$Cp[valid_indices])],
  LOOCV = valid_indices[which.min(criteria$LOOCV[valid_indices])]
)

# 印出最佳模型資訊
for (i in seq_along(best_models)) {
  idx <- best_models[[i]]
  cat("\nBest model for", names(best_models)[i], ":\n")
  cat("Number of predictors:", num_predictors[idx], "\n")
  cat("Predictors:", paste(colnames(X)[predictor_indices[[idx]]], collapse = ", "), "\n")
}

valid_indices <- which(num_predictors > 0)  
valid_criteria <- criteria[valid_indices, ]  
valid_num_predictors <- num_predictors[valid_indices] 

criteria_names <- c("CenteredR2", "AdjustedR2", "AIC", "BIC", "Cp", "LOOCV")
for (crit_name in criteria_names) {
  png(filename = paste0(crit_name, "_plot.png"), width = 1000, height = 600)
  
  par(mar = c(4.5, 4.5, 3, 1))
  
  y_values <- valid_criteria[[crit_name]]  
  
  if (crit_name %in% c("CenteredR2", "AdjustedR2")) {
    best_idx <- which.max(y_values)
    highlight = "red"
  } else {
    best_idx <- which.min(y_values)
    highlight = "blue"
  }
  
  plot(valid_num_predictors, y_values, 
       main = paste(crit_name, "vs Number of Predictors"),
       xlab = "Number of Predictors", ylab = crit_name,
       pch = 16, col = "gray", cex = 1.2)
  
  points(valid_num_predictors[best_idx], y_values[best_idx], 
         col = highlight, pch = 16, cex = 2)
  points(valid_num_predictors[best_idx], y_values[best_idx], 
         col = highlight, pch = 1, cex = 3)
  
  legend("topright", 
         legend = c("Models", paste("Best model (", valid_num_predictors[best_idx], " predictors)", sep = "")),
         pch = c(16, 16), col = c("gray", highlight), pt.cex = c(1, 2), bg = "white")
  
  dev.off()
}



