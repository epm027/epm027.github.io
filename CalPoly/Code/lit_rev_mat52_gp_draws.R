## FOUR SETS OF 5 DRAWS FROM A GP WITH 
## SQUARED-EXPONENTIAL KERNEL

get_plot <- function(ell) {
  dists <- abs(outer(x, x, FUN = "-"))
  t1 <- sqrt(5) * dists / ell
  t2 <- (5 * dists^2) / (3 * ell^2)
  t3 <- exp(-sqrt(5) * dists / ell)
  K <- (1 + t1 + t2) * t3
  
  y1 <- mvrnorm(n = numdraws,
                mu = rep(0, B),
                Sigma = K) %>%
    t() %>%
    data.frame()
  
  colnames(y1) <- paste0("draw",1:numdraws) 
  y1 %>%
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
    ggtitle(paste0("Matérn-5/2; Ell = ", ell))
}