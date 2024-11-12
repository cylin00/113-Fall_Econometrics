library(MASS)
set.seed(12345)

ns <- c(50, 100, 200, 500)
k <- 5
nominal <- 0.05
rep <- 1000
R <- rbind(c(1, 0, 0, 0, 0, 0), c(1, 1, 0, 0, 0, 0))

MeanVector <- rep(0, k + 1)
Covariance <- diag(k + 1)

IidSeq <- lapply(ns, function(n) {
  mvrnorm(n = n, mu = MeanVector, Sigma = Covariance)
})

names(IidSeq) <- paste0("n_", ns)

