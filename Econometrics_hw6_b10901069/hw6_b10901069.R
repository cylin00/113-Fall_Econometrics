# -------------------- Problem 1 --------------------

Graph <- function(density_sim, n){
  x_vals <- seq(min(density_sim$x), max(density_sim$x), length.out = 200)
  theoretical_density <- dnorm(x_vals, mean = 0, sd = 1/sqrt(n))
  
  y_max <- max(c(density_sim$y, theoretical_density)) * 1.15 
  plot(density_sim, col = "blue", lwd = 2, 
       main = paste("Simulated vs. Theoretical (n =", n, ")"),
       xlab = "S_n values", ylab = "Density",
       ylim = c(0, y_max))  
  
  lines(x_vals, theoretical_density, col = "red", lwd = 2, lty = 2)
  
  legend("topright", cex = 0.6, legend = c("Simulated KDE", "Theoretical Normal Density"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2)
}

#

Simulate_Mean <- function(n, rep){
  Sim <- numeric(rep)
  
  for (r in 1:rep){
    Y <- rnorm(n, mean = 0, sd = 1)
    Sim[r] <- mean(Y)
  }
  
  SimDensity <- density(Sim)
  Graph(SimDensity, n)
}

#

set.seed(12345)
rep <- 1000

#

n1 <- 10
Simulate_Mean(n1, rep)


#

n2 <- 50
Simulate_Mean(n2, rep)

#

n3 <- 100
Simulate_Mean(n3, rep)

# -------------------- Problem 2 --------------------

Data <- read.csv("Equity_Premium.csv")