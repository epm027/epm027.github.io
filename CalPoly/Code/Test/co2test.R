library(tidyverse)
library(reshape2)

co2 <- read.csv("../../data/co2_mm_mlo.csv",
                header = T,
                skip = 40)

co2 %>% ggplot(aes(x = decimal.date,
                   y = average)) +
  geom_line(size = 0.5) +
  theme_minimal(base_size = 12) +
  ggtitle("Monthly Average CO2 Emissions")


source("../fit_proc.R")

cand_ell <- seq(0.01, 0.1, length = 100)
devs_co2 <- c()

for(ell in cand_ell){
  fit_temp <- fit_proc_ma_q(
    co2$decimal.date,
    co2$average,
    ma_order = 6,
    start_coef = c(10, 0.5, rep(0.1, 6)),
    ell = ell
  )
  
  devs_co2 <- c(devs_co2, fit_temp$dev)
}

ell_opt <- cand_ell[which.min(devs_co2)]

data.frame(x = cand_ell,
           y = devs_co2) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  theme_minimal(base_size = 12) +
  geom_vline(xintercept = ell_opt,
             linetype = "dashed", col = "blue") +
  annotate(
    "label",
    x = ell_opt, y = 60,
    label = ell_opt %>% round(6)
  )


fit_opt <- fit_proc_ma_q(
  co2$decimal.date,
  co2$average,
  ma_order = 12,
  start_coef = c(10, 0.5, rep(0.1, 12)),
  ell = ell_opt
)

co2 %>%
  mutate(fitted = fit_opt$fitted) %>%
  ggplot(aes(x = decimal.date)) +
  geom_line(aes(y = average, colour = "average")) +
  geom_line(aes(y = fitted, colour = "fitted")) +
  theme_minimal() + 
  labs(coloud = "Legend")
