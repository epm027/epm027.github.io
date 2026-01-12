## FOUR SETS OF 5 DRAWS FROM A GP WITH 
## SQUARED-EXPONENTIAL KERNEL

B <- 500
numdraws <- 5
x <- seq(0, 10, length = B)
set.seed(927)

K1 <- exp(-outer(x, x, FUN = "-")^2 / (2 * 0.5^2))
K2 <- exp(-outer(x, x, FUN = "-")^2 / (2 * 1^2))
K3 <- exp(-outer(x, x, FUN = "-")^2 / (2 * 2^2))
K4 <- exp(-outer(x, x, FUN = "-")^2 / (2 * 10^2))

y1 <- mvrnorm(n = numdraws,
              mu = rep(0, B),
              Sigma = K1) %>%
  t() %>%
  data.frame()

colnames(y1) <- paste0("draw",1:numdraws) 

y2 <- mvrnorm(n = numdraws,
              mu = rep(0, B),
              Sigma = K2) %>%
  t() %>%
  data.frame()

colnames(y2) <- paste0("draw",1:numdraws) 

y3 <- mvrnorm(n = numdraws,
              mu = rep(0, B),
              Sigma = K3) %>%
  t() %>%
  data.frame()

colnames(y3) <- paste0("draw",1:numdraws) 

y4 <- mvrnorm(n = numdraws,
              mu = rep(0, B),
              Sigma = K4) %>%
  t() %>%
  data.frame()

colnames(y4) <- paste0("draw",1:numdraws) 


p1 <- y1 %>%
  melt(
    id.vars = c(),
    variable.name = 'draw',
    value.name = 'y'
  ) %>%
  mutate(
    x = rep(x, numdraws)
  ) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y, group = draw, colour = draw), size = 1) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  ggtitle("SE Kernel; Ell = 0.5")

p2 <- y2 %>%
  melt(
    id.vars = c(),
    variable.name = 'draw',
    value.name = 'y'
  ) %>%
  mutate(
    x = rep(x, numdraws)
  ) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y, group = draw, colour = draw), size = 1) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  ggtitle("SE Kernel; Ell = 1")

p3 <- y3 %>%
  melt(
    id.vars = c(),
    variable.name = 'draw',
    value.name = 'y'
  ) %>%
  mutate(
    x = rep(x, numdraws)
  ) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y, group = draw, colour = draw), size = 1) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  ggtitle("SE Kernel; Ell = 2")

p4 <- y4 %>%
  melt(
    id.vars = c(),
    variable.name = 'draw',
    value.name = 'y'
  ) %>%
  mutate(
    x = rep(x, numdraws)
  ) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y, group = draw, colour = draw), size = 1) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  ggtitle("SE Kernel; Ell = 10")

grid.arrange(p1, p2, p3, p4, ncol = 2)