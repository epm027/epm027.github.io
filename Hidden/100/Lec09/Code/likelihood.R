lik1 <- Vectorize(function(N, t, m, r){
  ## N = parameter of interest
  ## t = num. tagged fish
  ## m = size of second sample
  ## r = num. recaptured fish
  
  #~ Restrict Parameters
  if((t %% 1 != 0) | (m %% 1 != 0) | (r %% 1 != 0)){
    stop("Parameters must be integer-valued")
  } else if((t >= N)){
    return(0)    # t must be less than N
  } else if(m >= N){
    return(0)    # m must be less than N
  } else if(N %% 1 != 0){
    return(0)
  }
  
  else if((r < max(0, m + t - N)) | (r > min(m, t))){
    return(0)
  } 
  
  else if(N <= max(m, t)){
    return(0)
  }
  
  else{
    return(
      (choose(t, r) * choose(N - t, m - r)) /
        (choose(N, m))
    )
  }
})

x <- seq(40, 100, by = 0.01)
y <- lik1(seq(40, 100, by = 0.01), 40, 30, 20)


library(tidyverse)
p1 <- data.frame(x, y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(col = "blue", size = 0.5) +
  theme_bw() +
  xlab("N") + ylab("Likelihood at N") +
  ggtitle("Likelihood; t = 40, m = 30, r = 20")
