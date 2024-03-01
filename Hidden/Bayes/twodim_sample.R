library(tidyverse)

n1 <- 30
  
pop <- expand_grid(
  x = 1:13,
  y = 1:13
)

pop %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

srs <- pop %>% slice_sample(n = n1)

data.frame() %>%
  ggplot() +
  geom_point(aes(x = pop$x, y = pop$y)) +
  geom_point(aes(x = srs$x, y = srs$y),
             col = "red",
             size = 3) +
  theme_minimal(base_size = 12) +
  xlab("x") + ylab("y")


## assign clusters

pop %>%
  filter((x %in% 1:4) & )

