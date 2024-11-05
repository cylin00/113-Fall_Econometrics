# -------------------- Problem 1 --------------------

Graph_Norm <- function(sim, density_sim, n, s, g){
  x_vals <- seq(-5, 5, length.out = 500)
  normal_density <- dnorm(x_vals)
  y_max <- max(c(density_sim$y)) * 1.15 
  
  png(paste(g, s, n, ".png", sep = "_"))
  
  plot(density_sim, col = "blue", lwd = 2, 
       main = paste("Simulated vs. Theoretical ( n =", n, ")"),
       xlab = "S_n values", ylab = "Density",
       xlim = c(-6, 6),
       ylim = c(0, y_max))  
  
  lines(x_vals, normal_density, col = "red", lwd = 2, lty = 2)
  
  legend("topright", cex = 0.6, legend = c("Simulated KDE", "Theoretical Density"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  
  dev.off()
}

#

Simulate_Mean <- function(n, rep){
  Sim <- numeric(rep)
  Sim_T <- numeric(rep)
  
  for (r in 1:rep){
    Y <- rnorm(n, mean = 0, sd = 1)
    Sim[r] <- mean(Y)
    
    Y_T <- rt(n, df = 2)
    Sim_T[r] <- mean(Y_T)
  }
  
  SimDensity <- density(Sim)
  SimDensity_T <- density(Sim_T)
  Graph_Norm(Sim, SimDensity, n, "Mean", "N")
  Graph_Norm(Sim_T, SimDensity_T, n, "Mean", "T")
}

#

Simulate_Scaled <- function(n, rep){
  Sim <- numeric(rep)
  Sim_T <- numeric(rep)
  
  for (r in 1:rep){
    Y <- rnorm(n, mean = 0, sd = 1)
    Sim[r] <- sqrt(n) * mean(Y)
    
    Y_T <- rt(n, df = 2)
    Sim_T[r] <- sqrt(n) * mean(Y)
  }
  
  SimDensity <- density(Sim)
  SimDensity_T <- density(Sim_T)
  Graph_Norm(Sim, SimDensity, n, "Scaled", "N")
  Graph_Norm(Sim_T, SimDensity_T, n, "Scaled", "T")
}

#

Simulate_Mean_T <- function(n, rep){
  Sim <- numeric(rep)
  
  for (r in 1:rep){
    Y <- rt(n, df = 2)
    Sim[r] <- mean(Y)
  }
  
  SimDensity <- density(Sim)
  Graph_T(Sim, SimDensity, n, "Mean")
}

#

Simulate_Scaled_T <- function(n, rep){
  Sim <- numeric(rep)
  
  for (r in 1:rep){
    Y <- rt(n, df = 2)
    Sim[r] <- sqrt(n) * mean(Y)
  }
  
  SimDensity <- density(Sim)
  Graph_T(Sim, SimDensity, n, "Mean")
}

#

set.seed(12345)
rep <- 1000

#

n1 <- 10
Simulate_Mean(n1, rep)
Simulate_Scaled(n1, rep)

#

n2 <- 50
Simulate_Mean(n2, rep)
Simulate_Scaled(n2, rep)

#

n3 <- 100
Simulate_Mean(n3, rep)
Simulate_Scaled(n3, rep)

# -------------------- Problem 2 --------------------

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
X_ep <- Matrix[, 11, drop = FALSE]
X_bmr <- Matrix[, 12, drop = FALSE]
X_ntis <- Matrix[, 13, drop = FALSE]
X <- cbind(X1, X_dfy, X_infl, X_svar, X_tms, X_tbl, X_dfr, X_db, X_ltr, X_ep, X_bmr, X_ntis)

#

UpdateName <- function(coef){
  new_names <- names(coef)
  new_names[1] <- "constant"  
  new_names <- sub("^Xx_", "x_", new_names) 
  new_names <- sub("^X", "", new_names) 
  names(coef) <- new_names
  
  return(coef)
}

#

ChangeName <- function(summary_table) {
  new_names <- paste0(seq_len(nrow(summary_table)))
  rownames(summary_table) <- new_names
  return(summary_table)
}

#

library(car)
alpha = 0.05

UpdatedX <- t(ChangeName(t(X)))
model <- lm(Y ~ (UpdatedX - 1))
coef <- summary(model)$coefficients

test <- data.frame(p_value = coef[, "Pr(>|t|)"])
test$GreaterThanAlpha <- test$p_value > alpha
test



c1 <- "UpdatedX1 = 0"
c2 <- "UpdatedX2 + UpdatedX3 = 0"
constraints <- c(c1, c2)
wald <- linearHypothesis(model, constraints, level = alpha)
wald


