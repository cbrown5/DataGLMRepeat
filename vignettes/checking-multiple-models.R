## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, fig.width = 8)

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
library(visreg)

m1 <- glm(y ~ x1 + x2, data = dat, 
           family = "poisson")

visreg(m1, scale = "response")


## -----------------------------------------------------------------------------
library(DataGLMRepeat)

dat <- dplyr::bind_cols(dat, data.frame(Estimate = predict(m1, type = "response")))
m1_resids <- with(dat, ds_resids(y, Estimate, family = "poisson"))

## -----------------------------------------------------------------------------
library(dplyr)
dfsim <- expand.grid(form = c("y ~ x1", "y ~ x1 + x2"), 
                     family = c("poisson", "quasipoisson"), 
                     stringsAsFactors = FALSE) %>% 
  mutate(ID = 1:n()) %>%
  as_tibble()

dout <- dfsim %>%
  group_by(ID) %>%
  with_groups({
    m1 <- glm(formula(form),
              family = family,
              data = dat)
    tibble(model = list(m1))
  }) %>%
  bind_rows(.id = "groups") %>%
  bind_cols(dfsim, .)

## -----------------------------------------------------------------------------
library(ggplot2)
gout <- dout %>%
  group_by(ID) %>%
  with_groups({
    p <- bind_cols(dat, 
                data.frame(Estimate = predict(model[[1]], type = "response")))
    p$ds <- ds_resids(dat$y, p$Estimate, plotds = FALSE, 
              family = "poisson")
    g1 <- ggplot(p) + 
      aes(x = Estimate, y = ds, color = x2) + 
      geom_point() + 
      ggtitle(paste_(form, family))
    list(g1)
  })
gout[[3]]

## -----------------------------------------------------------------------------
library(purrr)
daic <- dout$model %>% 
  map(~AIC(.x)) %>%
  unlist()
dfsim[which.min(daic),]

## ----eval=FALSE---------------------------------------------------------------
#  library(brms)
#  library(patchwork)
#  
#  m1 <- brm(y ~ x1 + x2, data = dat,
#             family = "poisson")
#  
#  ce <- conditional_effects(m1)
#  
#  plot(ce, plot = FALSE)[[1]] | plot(ce, plot = FALSE)[[2]]
#  

## ----eval=FALSE---------------------------------------------------------------
#  dat <- dplyr::bind_cols(dat, data.frame(predict(m1)))
#  m1_resids <- with(dat, ds_resids(y, Estimate, family = "poisson"))

## ----eval=FALSE---------------------------------------------------------------
#  dfsim <- expand.grid(form = c("y ~ x1", "y ~ x1 + x2"),
#                       prior = c("normal(0, 0.1)", "normal(0, 10)"),
#                       stringsAsFactors = FALSE) %>%
#    mutate(ID = 1:n()) %>%
#    as_tibble()
#  
#  dout <- dfsim %>%
#    group_by(ID) %>%
#    with_groups({
#      m1 <- brm(formula(form),
#                family = "poisson",
#                prior = set_prior(prior),
#                data = dat)
#  
#      tibble(model = list(m1))
#    }) %>%
#    bind_rows(.id = "groups") %>%
#    bind_cols(dfsim, .)

## ----eval=FALSE---------------------------------------------------------------
#  gout <- dout %>%
#    group_by(ID) %>%
#    with_groups({
#      p <- bind_cols(dat,
#                  data.frame(predict(model[[1]])))
#      p$ds <- ds_resids(dat$y, p$Estimate, plotds = FALSE,
#                family = "poisson")
#      g1 <- ggplot(p) +
#        aes(x = Estimate, y = ds, color = x2) +
#        geom_point() +
#        ggtitle(paste_(form, prior))
#      list(g1)
#    })
#  gout[[3]]

## ----eval=FALSE---------------------------------------------------------------
#  dloo <- dout$model %>%
#    map(~loo(.x)$estimate[3,1]) %>%
#    unlist()
#  dfsim[which.min(dloo),]

