set.seed(10)
ell <- 0.5
sig0 <- 0.5

n <- 10
x <- seq(0, 10, length = n)
y <- sin(2 * pi * x / 10) + rnorm(n, 0, sig0)

xinit <- x
yinit <- y

xgrid <- seq(0, 10, length = 100)
m <- length(xgrid)

K_00 <- exp(-outer(x, x, FUN = "-")^2 / 2 * (ell^2))
K_S0 <- exp(-outer(xgrid, x, FUN = "-")^2 / 2 * (ell^2))
K_0S <- exp(-outer(x, xgrid, FUN = "-")^2 / 2 * (ell^2))
K_SS <- exp(-outer(xgrid, xgrid, FUN = "-")^2 / 2 * (ell^2))

post_mean <- K_S0 %*% solve(K_00 + sig0^2 * diag(1, n)) %*% y
post_var <- K_SS - K_S0 %*% solve(K_00 + sig0^2 * diag(1, n)) %*% K_0S

B <- 5
p1 <- mvrnorm(n = B,
              mu = post_mean,
              Sigma = post_var
)%>%
  t() %>%
  data.frame() %>%
  melt(
    id.vars = c(),
    value.name = "y",
    variable.name = "draw"
  ) %>%
  mutate(
    x = rep(xgrid, B)
  ) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y, group = draw, colour = "Post. Draws"),
            linetype = "dotted",
            linewidth = 0.5) +
  stat_function(fun = \(x) sin(2 * pi * x / 10),
                linewidth = 1,
                aes(colour = "Truth")) +
  geom_ribbon(
    inherit.aes = FALSE,
    data = data.frame(x = xgrid,
                      upper = post_mean + 1.96 * sqrt(diag(post_var)),
                      lower = post_mean - 1.96 * sqrt(diag(post_var))),
    aes(x = x, ymin = lower, ymax = upper),
    alpha = 0.1
  ) +
  geom_line(
    inherit.aes = FALSE,
    data = data.frame(x = xgrid, y = post_mean),
    aes(x = xgrid, y = post_mean, colour = "Post. Mean"),
    linetype = "longdash",
    linewidth = 1) +
  geom_point(
    inherit.aes = FALSE,
    data = data.frame(xinit, yinit),
    aes(x = xinit, y = yinit, colour = "Training"),
    size = 5,
    shape = 13
  ) +
  theme_minimal(base_size = 10)
