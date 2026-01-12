source("Code/signal_draws.R")

get_signal_plot <- function(n, 
                            ell,
                            sigma_signal = 1,
                            kernel_type,
                            base_size = 12) {
  data.frame(
    x1 = signal_draw(n = n, ell = ell, kernel_type = kernel_type, sigma_signal = sigma_signal),
    x2 = signal_draw(n = n, ell = ell, kernel_type = kernel_type, sigma_signal = sigma_signal),
    x3 = signal_draw(n = n, ell = ell, kernel_type = kernel_type, sigma_signal = sigma_signal),
    x4 = signal_draw(n = n, ell = ell, kernel_type = kernel_type, sigma_signal = sigma_signal),
    x5 = signal_draw(n = n, ell = ell, kernel_type = kernel_type, sigma_signal = sigma_signal)
  ) %>%
    melt(
      variable.name = "draw"
    ) %>% 
    ggplot(aes(x = rep(1:n, 5))) +
    geom_line(aes(y = value, colour = draw),
              linewidth = 1) +
    theme_minimal(base_size = base_size) +
    xlab("x") + ylab("y") +
    theme(plot.title = element_text(face = "bold")) +
    ggtitle(paste(ifelse(kernel_type == "matern", "Matérn", "SE"), "Draws; ell =", ell)) +
    theme(legend.position = "none")
}

