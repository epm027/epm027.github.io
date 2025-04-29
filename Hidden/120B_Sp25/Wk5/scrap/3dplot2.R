library(tidyverse)
library(plotly)
library(tidybayes)

x <- seq(-3, 3, length = 100)
y <- seq(-3, 3, length = 100)

xygrid <- expand.grid(x, y)

f <- function(v){
  x <- v[1]
  y <- v[2]
  if((0 <= x) & (x <= 1) & (x <= y)) {
    return(2 * (x + y))
  } else {
    return(0)
  }
}

dat <- data.frame(
  xygrid,
  apply(xygrid, MARGIN = 1, FUN = f)
)

colnames(dat) <- c("x", "y", "z")

dat %>% plot_ly(
  x = ~x,
  y = ~y
) %>%
  add_surface()
