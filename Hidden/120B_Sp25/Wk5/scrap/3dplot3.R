n <- 250

f <- Vectorize(function(x, y){
  if((0 <= x) & (x <= 1) & (0 <= y) & (y <= 1) & (x <= y)) {
    return(2 * (x + y))
  } else {
    return(0)
  }
})

x <- seq(-0.5, 1.5, length = n)
y <- seq(-0.5, 1.5, length = n)

z <- matrix(rep(NA, n^2), nrow = n)
for(i in 1:n) {
  for(j in 1:n) {
    z[i, j] <- f(x[i], y[j])
  }
}

plot_ly(x = ~x, y = ~y, z = ~z)  %>%
  add_surface(
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor="#ff0000",
        project = list(z = TRUE)
      ),
      x = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor="#ff0000",
        project = list(z = TRUE)
      ),
      y = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor="#ff0000",
        project = list(z = TRUE)
      )
    )
  ) 