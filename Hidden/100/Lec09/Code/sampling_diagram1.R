library(tidyverse)
library(reshape2)
set.seed(123)

pop <- 1:100
n <- 20

## SRS
srs <- sample(pop, size = n)

## STRATIFIED
strat_samp <- c()
for(k in seq(1, 100, by = 10)) {
  strat_samp <- c(strat_samp, 
                  sample(pop[k : (k + 9)], size = 2)
  )
}


## CLUSTER
clust_samp <- c()
sampled_starts <- sample(seq(1, 100, by = 5), size = 10)

for(k in sampled_starts) {
  clust_samp <- c(clust_samp, 
                  sample(pop[k : (k + 4)], size = 2)
  )
}


## COMBINE

data.frame(
  srs = srs,
  stratified = strat_samp,
  cluster = clust_samp
) %>% 
  melt(
    variable.name = 'type'
  ) %>%
  ggplot(aes(x = value, y = type)) + 
  geom_point() +
  theme_minimal() +
  ggtitle("Three Samples of Size 20",
          subtitle = "Grouped by Sampling Technique")
