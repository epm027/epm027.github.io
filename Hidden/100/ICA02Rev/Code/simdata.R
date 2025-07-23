library(tidyverse)
library(MASS)

n <- 125
set.seed(100)

mu <- c(4, 5)
Sig <- matrix(c(10, 6, 6, 8), nrow = 2, byrow = T)

X1 <- mvrnorm(n = n, 
        mu = mu,
        Sigma = Sig)
x3 <- rnorm(n, 5, 9)

X <- cbind(X1, x3)
df1 <- data.frame(X)
names(df1) <- c("x1", "x2", "x3")

y <- 0.5 + 1.2 * df1$x1 + 1.8 * df1$x2 + 2.1 * df1$x3 + rnorm(n)
df1 <- df1 %>% mutate(y = y)

###

lm(y ~ ., df1) %>% summary()
