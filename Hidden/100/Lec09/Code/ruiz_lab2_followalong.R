library(tidyverse)

## CONSTRUCT THE POPULATION
set.seed(40221)
population <- data.frame(
  data = rgamma(5000, 2, scale = 1/2),
  seed = 1:5000
)

mean_pop_diameter <- mean(population$data)
mean_pop_diameter

std_dev_pop_diameter <- sd(population$data)
std_dev_pop_diameter

population %>% 
  ggplot(aes(x = data)) + 
  geom_histogram(bins = 20, col = "white", alpha = 0.8) +
  geom_vline(xintercept = mean_pop_diameter,
             col = "blue") + 
  theme_minimal()


## DRAW A SAMPLE
set.seed(40221) # for reproducibility
sample <- population %>% slice_sample(n = 250)

mean_sample_diameter <- mean(sample$data)
mean_sample_diameter


rbind(sample, population) %>% 
  mutate(
    ind = c(rep("sample", 250), rep("population", 5000))
  ) %>%
  ggplot(aes(x = data)) + 
  geom_histogram(aes(y = after_stat(density), fill = ind),
                 bins = 20, alpha = 0.8, col = 'white') +
  geom_vline(xintercept = mean_pop_diameter,
             col = "salmon") + 
  geom_vline(xintercept = mean_sample_diameter,
             col = "blue") + 
  theme_minimal()

## TAKE 1000 SAMPLES OF SIZE 300

B <- 1000
samp_means <- c()
for(k in 1:B){
  temp_samp <- population %>% slice_sample(n = 300)
  samp_means <- c(samp_means, mean(temp_samp$data))
}

mean(samp_means) - mean_pop_diameter
sd(samp_means)

data.frame(samp_means) %>%
  ggplot(aes(x = samp_means)) +
  geom_histogram(aes(y = after_stat(density)),
                     col = "white", alpha = 0.8,
                 bins = 20)

## NOW, SAMPLE ACCORDING TO A BIASED SCHEME
## SMALLER SEEDS HAVE A LOWER CHANCE OF BEING INCLUDED

population_mod1 <- population

weight_fnt <- function(x, r = 10, c = 1.5) {
  return(1 / (1 + exp(-r * (x - c))))
}

grid <- seq(0, 6, length = 100)

weight_df <- data.frame(
  `seed diameter` = grid,
  weight = weight_fnt(grid)
)

data.frame(x = 0:6) %>%
  ggplot(aes(x = x)) +
  stat_function(fun = weight_fnt) +
  theme_minimal() +
  ggtitle("Weight Prob. vs. Diameter")

population %>% 
  ggplot(aes(x = data)) + 
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20, col = "white", alpha = 0.8) +
  geom_vline(xintercept = mean_pop_diameter,
             col = "blue") + 
  theme_minimal() + 
  stat_function(fun = weight_fnt) 

population_mod1 <- population_mod1 %>%
  mutate(
    weight = weight_fnt(data)
  )

set.seed(40721)
sample2 <- population_mod1 %>% slice_sample(n = 250, 
                                            weight_by = weight)

mean_sample2_diameter <- mean(sample2$data)
mean_sample2_diameter

mean_pop2_diameter <- mean(population_mod1$data)

rbind(sample2, population_mod1) %>% 
  mutate(
    ind = c(rep("sample", 250), rep("population", 5000))
  ) %>%
  ggplot(aes(x = data)) + 
  geom_histogram(aes(y = after_stat(density), fill = ind),
                 bins = 20, alpha = 0.8, col = 'white') +
  geom_vline(xintercept = mean_pop2_diameter,
             col = "salmon") + 
  geom_vline(xintercept = mean_sample2_diameter,
             col = "blue") + 
  facet_wrap(~ind) +
  theme_bw()


B <- 1000
samp_means2 <- c()
for(k in 1:B){
  temp_samp <- population_mod1 %>% slice_sample(n = 300)
  samp_means2 <- c(samp_means2, mean(temp_samp$data))
}

mean(samp_means2) - mean(population_mod1$data)