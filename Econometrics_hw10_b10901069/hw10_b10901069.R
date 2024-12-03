rm(list = ls())
library(stats)
library(ggplot2)
library(numDeriv) 

Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
x_dfy <- Matrix[, 3, drop = FALSE]
x_infl <- Matrix[, 4, drop = FALSE]
x_svar <- Matrix[, 5, drop = FALSE]
x_tms <- Matrix[, 6, drop = FALSE]
x_tbl <- Matrix[, 7, drop = FALSE]
x_dfr <- Matrix[, 8, drop = FALSE]
x_dp <- Matrix[, 9, drop = FALSE]
x_ltr <- Matrix[, 10, drop = FALSE]
x_ep <- Matrix[, 11, drop = FALSE]
x_bmr <- Matrix[, 12, drop = FALSE]
x_ntis <- Matrix[, 13, drop = FALSE]
X <- cbind(x_dfy, x_infl, x_svar, x_tms, x_tbl, x_dfr, x_dp, x_ltr, x_ep, x_bmr, x_ntis)

# Bull Market from 1981 - 2022
Y <- matrix(1, nrow = length(x_dfy), ncol = 1)
Y[20:80] <- 0 # August 1982 - August 1987
Y[84:231] <- 0 # December 1987 - March 2000
Y[249:253] <- 0 # September 2001 - January 2002
Y[262:322] <- 0 # October 2002 - October 2007
Y[339:470] <- 0 # March 2009 - February 2020
Y[471:493] <- 0 # March 2020 - January 2022
Y[494:500] <- 0 # February 2022 - August 2022


probit_function <- function(beta, X) {
  pnorm(X %*% beta)
}

logit_function <- function(beta, X) {
  1 / (1 + exp(-X %*% beta))
}

probit_loglik <- function(beta, X, Y) {
  p <- probit_function(beta, X)
  -sum(Y * log(p) + (1 - Y) * log(1 - p)) 
}

logit_loglik <- function(beta, X, Y) {
  p <- logit_function(beta, X)
  -sum(Y * log(p) + (1 - Y) * log(1 - p)) 
}

X_with_intercept <- cbind(1, X)
colnames(X_with_intercept) <- c("Intercept", colnames(X)) 

linear_model <- lm(Y ~ X) # Linear regression model
beta_ls <- coef(linear_model)

probit_mle <- optim(
  par = beta_ls,
  fn = probit_loglik,
  X = X_with_intercept, 
  Y = Y,
  method = "BFGS",
  control = list(maxit = 10000) 
)

logit_mle <- optim(
  par = beta_ls,
  fn = logit_loglik,
  X = X_with_intercept,
  Y = Y,
  method = "BFGS", 
  control = list(maxit = 10000) 
)

probs_linear <- X_with_intercept %*% beta_ls
probs_probit <- probit_function(probit_mle$par, X_with_intercept)
probs_logit <- logit_function(logit_mle$par, X_with_intercept)

plot_data <- data.frame(
  Index = 1:nrow(X),
  Y = Y,
  Linear = probs_linear,
  Probit = probs_probit,
  Logit = probs_logit
)

ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Y, color = "Observed Y")) +
  geom_line(aes(y = Linear, color = "Linear")) +
  geom_line(aes(y = Probit, color = "Probit")) +
  geom_line(aes(y = Logit, color = "Logit")) +
  labs(y = "Probability", color = "Model") +
  theme_minimal()

probit_score <- numDeriv::grad(probit_loglik, probit_mle$par, X = X_with_intercept, Y = Y)
logit_score <- numDeriv::grad(logit_loglik, logit_mle$par, X = X_with_intercept, Y = Y)

names(probit_score) <- colnames(X_with_intercept)
names(logit_score) <- colnames(X_with_intercept)

print(probit_score)
print(logit_score)

