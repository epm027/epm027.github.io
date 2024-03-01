library(tidyverse)

B <- 1000
counts <- 0
n <- 100

for(j in 1:B){
  x <- sample(1:n, 1)
  y <- sample(1:n, 1)
  if(x < y) {
    counts <- counts + 1
  }
}

counts/B
(n + 1)/(2*n)



###
props <- c()

for(j in 1:1000){
  x <- sample(1:n, B, replace = T)
  y <- sample(1:n, B, replace = T)
  props <- c(props, sum(x <= y) / B)
}