rm(list = ls())
library(stats)

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
