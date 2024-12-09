rm(list = ls())
library(forecast)
library(ggplot2)

Data <- read.csv("Equity_Premium.csv")
Matrix <- as.matrix(Data)
Y <- Matrix[, 2, drop = FALSE]
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

combined_data <- cbind(X, Y)
colnames(combined_data) <- c(paste0("X_", 1:ncol(X)), "Y")

acf_data <- data.frame(Lag = integer(), ACF = numeric(), Series = character())

for (i in 1:ncol(combined_data)) {
  ts_data <- combined_data[, i]
  series_name <- colnames(combined_data)[i]
  
  acf_result <- acf(ts_data, lag.max = 24, plot = FALSE)
  acf_values <- acf_result$acf[-1]
  lags <- 1:24
  
  acf_data <- rbind(acf_data, data.frame(Lag = lags, ACF = acf_values, Series = series_name))
}

conf_level <- qnorm(0.975) / sqrt(nrow(combined_data))

acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF, color = Series, group = Series)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(conf_level, -conf_level), linetype = "dashed", color = "black") +
  labs(title = "Autocorrelation Functions of All Time Series",
       x = "Lag",
       y = "Autocorrelation") +
  theme_minimal() +
  theme(legend.title = element_blank()) # Remove legend title

ggsave("Combined_ACF_Plot.png", plot = acf_plot, width = 10, height = 6)

print(acf_plot)
