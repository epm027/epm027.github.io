library(tidyverse)
B <- 500

S <- list(c(1, 2), c(2, 3), c(1, 3))
X <- sample(S, size = B, replace = TRUE, prob = c(1/2, 1/6, 1/3))

dfnana <- data.frame(x = unlist(X)[seq(1, 2*B, by = 2)], 
           y = unlist(X)[seq(2, 2*B, by = 2)]
)

paste0(dfnana$x,",",dfnana$y) %>% table() / B

###

sampmeans <- apply(dfnana, MARGIN = 1, FUN = mean)
barplot(table(sampmeans))
mean(sampmeans)

## raw sample mean is biased!!!!

pi1 <- 2/3
pi2 <- 5/6
pi3 <- 1/2

w1 <- (1/pi1) / ((1/pi1) + (1/pi2) + (1/pi3))
w2 <- (1/pi2) / ((1/pi1) + (1/pi2) + (1/pi3))
w3 <- (1/pi3) / ((1/pi1) + (1/pi2) + (1/pi3))

updated_mean <- 0

weight_tab <- data.frame(
  val = c(1, 2, 3),
  weight = c(w1, w2, w3)
)

dfnew <- dfnana %>% 
  left_join(
    weight_tab,
    by = join_by(x == val)
  ) %>% 
  left_join(
    weight_tab,
    by = join_by(y == val)
  ) %>%
  mutate(
    weighted_mean = x * weight.x + y * weight.y
  )

mean(dfnew$weighted_mean)
