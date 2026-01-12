library(tidyverse)
library(gganimate)

st_wave <- function(x) {x - 2 * round(x / 2)}

st_approx <- function(n, x) {
  tot <- 0
  for(k in 1:n) {
    tot <- tot + 2 / (k * pi) * (-1)^(k + 1) * sin(k * pi * x)
  }
  return(tot)
}

####
N <- 300
x <- seq(-1, 3, length = N)
df <- data.frame(x = x,
                 st = st_wave(x))

for(n in 1:30) {
  temp_vect <- st_approx(n, x)
  df <- cbind(df, temp_vect)
}

names(df) <- c("x", "truth", 1:30)

anim1 <- df %>% 
  melt(id.vars = c("x", "truth"),
       variable.name = "n") %>%
  ggplot(aes(x = x, y = value)) +
  geom_line(aes(y = truth,
                colour = "truth"),
            linewidth = 1) +
  geom_line(aes(colour = "approx"),
            linewidth = 2) +
  ggtitle("Approximation to a Sawtooth Wave",
          subtitle = "n = {frame}") +
  theme(plot.title = element_text(face = "bold")) +
  transition_states(n) +
  scale_color_okabe_ito() +
  theme_minimal(base_size = 24) +
  xlab("x") + ylab("f(x)") +
  labs(colour = "Legend") 