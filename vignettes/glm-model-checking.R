## ----setup, include=FALSE, results = FALSE------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## -----------------------------------------------------------------------------
n <- 50
a <- 1
beta <- cbind(1,2)
x <- rbind(runif(n), runif(n))
linpred <- a + beta %*% x
dat <- data.frame(x1 = x[1,],
                  x2 = x[2,], 
                  y = rpois(n, exp(linpred)))

## -----------------------------------------------------------------------------
library(brms)
library(patchwork)

m1 <- brm(y ~ x1 + x2, data = dat, 
           family = "poisson")

ce <- conditional_effects(m1)

plot(ce, plot = FALSE)[[1]] | plot(ce, plot = FALSE)[[2]]

## -----------------------------------------------------------------------------
library(ModelTools)

dat <- dplyr::bind_cols(dat, data.frame(predict(m1)))
m1_resids <- with(dat, ds_resids(y, Estimate, family = "poisson"))

