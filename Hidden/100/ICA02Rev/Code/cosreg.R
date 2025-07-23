library(tidyverse)
set.seed(100)
n <- 50

x <- (2 * (1:n) - 1) / (2*n)
y <- 1 * (x - 0.5)^3 - 0.25 * (x - 0.5)^2 - 0.1 * (x - 0.5) + rnorm(n, 0, 0.01)

cosest <- function(t, x, y, lam) {
  n <- length(y)
  ybar <- mean(y)
  est1 <- 0
  for(j in 2:lam){
    betaj <- 0
    for(i in 1:n) {
      betaj <- betaj + y[i] * sqrt(2) * cos((j - 1) * pi * x[i])
    }
    betaj <- (1/n) * betaj
    est1 <- est1 + betaj * sqrt(2) * cos((j - 1) * pi * t)
  }
  return(ybar + est1)
}


#### BW Selection with CV
cv_errors <- c()
cand_lam <- 3:20
for(lam in cand_lam) {
  cverr <- 0
  for(i in 1:n){
    xtrain <- x[-i]
    ytrain <- y[-i]
    
    predfnt <- \(t){cosest(t, xtrain, ytrain, lam)}
    cverr <- cverr + ((y[i] - predfnt(x[i]))^2)
  }
  cv_errors <- c(cv_errors, mean(cverr))
}

lam_opt <- cand_lam[which.min(cv_errors)]
