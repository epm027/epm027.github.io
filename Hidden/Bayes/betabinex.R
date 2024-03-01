library(tidyverse)

# (Y | pi) ~ Bin(10, pi)
# pi ~ Beta(2, 2)
# Observe: Y = 9

N <- 100
pi_grid <- seq(0, 1, length = N)
discrete_prior <- dbeta(pi_grid, 2, 2)
discrete_likelihood <- dbinom(9, 10, pi_grid)
post_probs <- (discrete_prior * discrete_likelihood) / sum((discrete_prior * discrete_likelihood))

sample(pi_grid, prob = post_probs, size = 10000, replace = T) %>%
  hist()
