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
colnames(combined_data) <- c(colnames(X), "Y")

ACFdata <- data.frame(Lag = integer(), ACF = numeric(), Series = character())
BPresults <- data.frame(Series = character(), Q_12 = numeric(), Q_24 = numeric(), Lag_12_Statistic = numeric(), Lag_24_Statistic = numeric())

for (i in 1:ncol(combined_data)) {
  ts_data <- combined_data[, i]
  series_name <- colnames(combined_data)[i]
  
  ACFresult <- acf(ts_data, lag.max = 24, plot = FALSE)
  ACFvalues <- ACFresult$acf[-1]
  lags <- 1:24
  
  ACFdata <- rbind(ACFdata, data.frame(Lag = lags, ACF = ACFvalues, Series = series_name))
  
  BP_test_12 <- Box.test(ts_data, lag = 12, type = "Box-Pierce")
  BP_test_24 <- Box.test(ts_data, lag = 24, type = "Box-Pierce")
  
  BPresults <- rbind(BPresults, data.frame(
    Series = series_name,
    Lag_12_P_Value = BP_test_12$p.value,
    Lag_24_P_Value = BP_test_24$p.value,
    Q_12 = BP_test_12$statistic,
    Q_24 = BP_test_24$statistic
  ))
}

conf_level <- qnorm(0.975) / sqrt(nrow(combined_data))

ACFplot <- ggplot(ACFdata, aes(x = Lag, y = ACF, color = Series, group = Series)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(conf_level, -conf_level), linetype = "dashed", color = "black") +
  labs(title = "Autocorrelation Functions of All Time Series",
       x = "Lag",
       y = "Autocorrelation") +
  theme_minimal() +
  theme(legend.title = element_blank()) # Remove legend title

ggsave("ACF_Plot.png", plot = ACFplot, width = 10, height = 6)
print(ACFplot)

cat("Box-Pierce Test Results:\n")
print(BPresults)
