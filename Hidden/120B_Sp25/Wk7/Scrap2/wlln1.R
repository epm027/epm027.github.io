library(tidyverse)
library(reshape2)
B <- 1000
K <- 50
p <- 0.25

X <- matrix(rep(NA, B * K), nrow = B)

for(k in 1:K){
  temp_samp <- sample(c(0, 1), size = B, replace = T, prob = c(1 - p, p))
  X[,k] <- cumsum(temp_samp) / (1:B)
}

X <- data.frame(X)

X %>% 
  melt(
    variable.name = "samp",
    value_name = "value"
  ) %>% mutate(
    x = rep(1:B, K)
    ) %>% ggplot(aes(x = x, y = value)) +
  geom_line(aes(group = samp), alpha = 0.3) +
  theme_minimal() + 
  xlab("num. trials") + ylab("Est. of p")


###


B <- 1000
K <- 50

sd_mat <- matrix(rep(NA, B * K), nrow = B)

for(k in 1:K){
  temp_samp <- rnorm(B, 0, 1.5)
  for(b in 1:B){
    sd_mat[b,k] <- sd(temp_samp[1:b])
  }
}

sd_mat <- data.frame(sd_mat)

sd_mat %>% 
  melt(
    variable.name = "samp",
    value_name = "value"
  ) %>% mutate(
    x = rep(1:B, K)
  ) %>% ggplot(aes(x = x, y = value)) +
  geom_line(aes(group = samp), alpha = 0.3) +
  theme_minimal() + 
  xlab("samp. size") + ylab("samp. sd")

##


B <- 100
K <- 50

matmat <- matrix(rep(NA, B * K), nrow = B)

for(k in 1:K){
  temp_samp <- runif(B, 0, 2.3)
  for(b in 1:B){
    matmat[b,k] <- max(temp_samp[1:b])
  }
}

matmat <- data.frame(matmat)

matmat %>% 
  melt(
    variable.name = "samp",
    value_name = "value"
  ) %>% mutate(
    x = rep(1:B, K)
  ) %>% ggplot(aes(x = x, y = value)) +
  geom_line(aes(group = samp), alpha = 0.3) +
  theme_minimal() + 
  xlab("samp. size") + ylab("samp. sd")
