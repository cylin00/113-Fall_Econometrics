library(MASS)
set.seed(12345)

ns <- c(50, 100, 200, 500)
k <- 5
nominal <- 0.05
rep <- 1000
R <- rbind(c(1, 0, 0, 0, 0), c(1, 1, 0, 0, 0))
theta <- rbind(1, 2)

MeanVector <- rep(0, k + 1)
Covariance <- diag(k + 1)

IidSeq <- lapply(ns, function(n) {
  mvrnorm(n = n, mu = MeanVector, Sigma = Covariance)
})

names(IidSeq) <- paste0("n_", ns)

# ------------------------- (a) -------------------------

simulate_chi_squared_test <- function(n, beta) {
  
  rejection_count <- 0
  
  for (i in 1:rep) {
    X <- matrix(rnorm(n * k), n, k)
    epsilon <- rnorm(n)
    
    Y <- X %*% beta + epsilon
    
    fit <- lm(Y ~ X - 1)  
    
    estimated_beta <- coef(fit)
    residuals <- Y - X %*% estimated_beta
    residual_variance <- sum(residuals^2) / (n - k)
    var_beta_hat <- residual_variance * solve(t(X) %*% X)
    test_statistic <- t(estimated_beta - beta) %*% solve(var_beta_hat) %*% (estimated_beta - beta)
    
    critical_value <- qchisq(1 - nominal, df = k)
    
    if (test_statistic > critical_value) {
      rejection_count <- rejection_count + 1
    }
  }
  
  empirical_size <- rejection_count / rep
  return(empirical_size)
}

beta1 <- rep(1, k)
empirical_sizes <- sapply(ns, function(n) simulate_chi_squared_test(n, beta1))

names(empirical_sizes) <- paste0("n_", ns)
print(empirical_sizes)

# ------------------------- (b) -------------------------

simulate_power_chi_squared_test <- function(n, beta_null, beta_alt) {
  
  rejection_count <- 0
  
  for (i in 1:rep) {
    X <- matrix(rnorm(n * k), n, k)
    epsilon <- rnorm(n)
    
    Y <- X %*% beta_alt + epsilon
    
    fit <- lm(Y ~ X - 1) 
    
    estimated_beta <- coef(fit)
    residuals <- Y - X %*% estimated_beta
    residual_variance <- sum(residuals^2) / (n - k)
    var_beta_hat <- residual_variance * solve(t(X) %*% X)
    test_statistic <- t(estimated_beta - beta_null) %*% solve(var_beta_hat) %*% (estimated_beta - beta_null)
    
    critical_value <- qchisq(1 - nominal, df = k)
    
    if (test_statistic > critical_value) {
      rejection_count <- rejection_count + 1
    }
  }
  
  empirical_power <- rejection_count / rep
  return(empirical_power)
}

beta2 <- c(1, 2, 3, 4, 5)
empirical_powers <- sapply(ns, function(n) simulate_power_chi_squared_test(n, beta1, beta2))

names(empirical_powers) <- paste0("n_", ns)
print(empirical_powers)


# ------------------------- (c) -------------------------

hs <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

beta3 <- function(h, n){
  beta <- rep(1+(n^(-0.5))*h, k)
  return(beta)
}

beta_list <- expand.grid(h = hs, n = ns)  
empiricalPowers <- apply(beta_list, 1, function(params) {
  h <- params["h"]
  n <- params["n"]
  beta_alt <- beta3(h, n)  
  simulate_power_chi_squared_test(n = n, beta_null = beta1, beta_alt = beta_alt)
})

empiricalPowers <- matrix(empiricalPowers, nrow = length(hs), ncol = length(ns),
                          dimnames = list(paste0("h_", hs), paste0("n_", ns)))
print(empiricalPowers)

