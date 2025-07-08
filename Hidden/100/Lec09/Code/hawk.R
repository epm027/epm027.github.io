library(tidyverse)
set.seed(100)

female_hawks <- data.frame(
  length = rnorm(n = 3000, 57.5, sd = 3),
  sex = rep('female', 3000),
  weight = rep(3/5, 3000)
)

male_hawks <- data.frame(
  length = rnorm(n = 2000, 50.5, sd = 3),
  sex = rep('male', 2000),
  weight = rep(2/5, 2000)
)

pop <- rbind(female_hawks, male_hawks)
samp <- slice_sample(pop, n = 100, weight_by = pop$weight) %>%
  select(!weight)

cat("True Pop Mean:", mean(pop$length))
cat("Sample Mean:", mean(samp$length))

## IPW

weight_df <- tibble(
  sex = c("male", "female"),
  weight = c(2/5, 3/5)
)

samp_w <- samp %>% 
  left_join(
    weight_df,
    by = "sex"
  ) %>% 
  mutate(
    corr_fact = (1/weight) / sum(1/weight),
    weighted_length = length * corr_fact
  ) %>% 
  summarise(
    weighted_mean = sum(weighted_length)
  ) %>% 
  pull(weighted_mean)

cat("True Pop Mean:", mean(pop$length))
cat("Sample Mean:", mean(samp$length))
cat("IPW Mean:", mean(samp_w))

##

table(samp$sex)
