library(tidyverse)
library(gridExtra)
library(ggthemes)

set.seed(27)

x <- rnorm(100, 5, 2)
y <- rnorm(100, x, 2) + 15

p1 <- data.frame(x, y) %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 4) +
  ylim(c(-5, 40)) +
  theme_economist_white(base_size = 14) +
  theme(
    panel.background = element_rect("white"),
    plot.background = element_rect(fill = "white"),
    axis.title.x = element_text(size = 16,
                                margin = margin(
                                  t = 20, 
                                  r = 0,
                                  b = 0, 
                                  l = 0)),
    axis.title.y = element_text(size = 16),
    title = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  ggtitle("Regression of Y on X")

p2 <- data.frame(x, y) %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 4) +
  ylim(c(-5, 40)) +
  theme_economist_white(base_size = 14) +
  theme(
    panel.background = element_rect("white"),
    plot.background = element_rect(fill = "white"),
    axis.title.x = element_text(size = 16,
                                margin = margin(
                                  t = 20, 
                                  r = 0,
                                  b = 0, 
                                  l = 0)),
    axis.title.y = element_text(size = 16),
    title = element_text(size = 18)
  ) +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  ggtitle("Regression of Y on X")

p3 <- data.frame(x, y) %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 4) +
  theme_economist_white(base_size = 14) +
  theme(
    panel.background = element_rect("white"),
    plot.background = element_rect(fill = "white"),
    axis.title.x = element_text(size = 16,
                                margin = margin(
                                  t = 20, 
                                  r = 0,
                                  b = 0, 
                                  l = 0)),
    axis.title.y = element_text(size = 16),
    title = element_text(size = 18)
  ) +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  ggtitle("Regression of Y on X")
