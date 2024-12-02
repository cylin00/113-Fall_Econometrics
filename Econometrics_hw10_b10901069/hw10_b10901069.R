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

Y <- matrix(1, nrow = length(x_dfy), ncol = 1)
# Bull Market from 1981 - 2022
Y[20:80] <- 0 # August 1982 - August 1987
Y[84:231] <- 0 # December 1987 - March 2000
Y[249:253] <- 0 # September 2001 - January 2002
Y[262:322] <- 0 # October 2002 - October 2007
Y[339:470] <- 0 # March 2009 - February 2020
Y[471:493] <- 0 # March 2020 - January 2022
Y[494:500] <- 0 # February 2022 - August 2022
