library(tidyverse)

n <- 100
B <- 100

set.seed(23)

lower_endpoints <- c()
upper_endpoints <- c()

for(j in 1:B) {
  X <- rnorm(100, 10, 1)
  lower_endpoints[j] <- mean(X) - qnorm(0.975) / sqrt(n)
  upper_endpoints[j] <- mean(X) + qnorm(0.975) / sqrt(n)
}

coverages <- rep("covers", B)
non_coverage_ind <- which(upper_endpoints < 10 | lower_endpoints > 10)
coverages[non_coverage_ind] <- c("fails to cover")
coverages <- factor(coverages)

p1 <- data.frame(lower = lower_endpoints,
           upper = upper_endpoints,
           index = 1:B,
           coverages = coverages) %>%
  ggplot(aes(y = index)) +
  geom_vline(xintercept = 10,
             col = "white",
             linewidth = 1) +
  geom_segment(aes(x = lower, xend = upper, 
                   y = index, yend = index,
                   colour = coverages),
               linewidth = 1) +
  theme_minimal(base_size = 14) +
  xlab("CI") + ylab("") +
  theme(
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white", size = 24),
    title = element_text(color = "white", size = 28),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(colour = "white",
                               size = 18),
    legend.title = element_text(size = 24)
  )
