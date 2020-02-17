## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## -----------------------------------------------------------------------------
library(dplyr)
library(DataGLMRepeat)
dat <- tibble(
  grp1 = sample(c(1, 2, 3), 100, replace = TRUE), 
  grp2 = sample(letters[1:3], 100, replace = TRUE), 
  x = (1:100)/100, 
  y = rpois(100, lambda = exp(grp1 + x*2)))

## -----------------------------------------------------------------------------
gout <- dat %>% 
  group_by(grp1, grp2) %>%
  with_groups(., {
    tibble(glmfit = 
      list(glm(y ~ x, family = "poisson")))
  }) %>% 
  bind_rows(.id = "group")

## -----------------------------------------------------------------------------
library(purrr)
gout$glmfit %>%
  map(~.x$coefficients[2]) %>%
  unlist()

