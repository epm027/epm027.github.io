library(tidyverse)

signal_draw <- function(
    n = 2^11,
    sigma_signal = 1,
    kernel_type = "matern",
    ell = 0.1
) {
  max_freq <- ceiling(n/2 - 1)
  
  if(kernel_type == "matern"){
    spec_dens <- function(k) {
      return(
        (16 * sqrt(5) / 15) * ell *
          (1 + (4*pi^2 * ell^2 / 5) * k^2)^(-3)
      )
    }
  } else if(kernel_type == "sq-exp"){
    spec_dens <- function(k) {
      return(
        sqrt(2 * pi * ell^2) * exp(-2 * pi^2 * ell^2 * k^2)
      )
    }
  }
  
  eta_r_0 <- rnorm(n = 1, 
                   mean = 0, 
                   sd = (sigma_signal) * sqrt(spec_dens(0))
  )
  
  eta_r_first <- rnorm(n = max_freq, 
                       mean = 0, 
                       sd = (sigma_signal)* sqrt(0.5 * spec_dens(1:max_freq))
  )
  
  eta_im_first <- rnorm(n = max_freq, 
                        mean = 0, 
                        sd = (sigma_signal)*sqrt(0.5 * spec_dens(1:max_freq))
  )
  
  eta_mid <- rnorm(n = 1, 
                   mean = 0, 
                   sd = (sigma_signal)*sqrt(spec_dens(n/2))
  )
  
  eta <- c(
    eta_r_0,
    complex(real = eta_r_first,
            imaginary = eta_im_first),
    eta_mid,
    rev(Conj(complex(real = eta_r_first,
                     imaginary = eta_im_first))
    )
  )
  
  if( n %% 2 == 0){
    eta <- c(
      eta_r_0,
      complex(real = eta_r_first,
              imaginary = eta_im_first),
      eta_mid,
      rev(Conj(complex(real = eta_r_first,
                       imaginary = eta_im_first))
      )
    )
    
  } else {
    eta <- c(
      eta_r_0,
      complex(real = eta_r_first,
              imaginary = eta_im_first),
      rev(Conj(complex(real = eta_r_first,
                       imaginary = eta_im_first))
      )
    )
  }
  
  signal <- fft(eta, inverse = T) %>% Re()
  
  return(signal)
}