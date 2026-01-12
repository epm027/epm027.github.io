## CODE THAT WRITES A FUNCTION THAT GENERATES
## DATA WITH MA NOISE

make_ma_data <- function(n = 2^11,
                         seed = 27,
                         sigma_0 = 0.5,
                         sigma_signal = 10,
                         kernel_type = "matern",
                         ell = 0.1,
                         ma_coefs = c(1/sqrt(2), 1/sqrt(2))) {
  set.seed(seed)
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
  
  ## COEFFICIENT GENERATION
  
  ## Generate alpha_0
  eta_r_0 <- rnorm(n = 1, 
                   mean = 0, 
                   sd = (sigma_signal^2)*sqrt(0.5 * spec_dens(0))
  )
  
  xi_r_0 <- rnorm(n = 1, 
                  mean = 0, 
                  sd = sigma_0
  )
  
  alpha_0 <- eta_r_0 + xi_r_0
  
  ## Generate alpha_1 through alpha_max_freq
  eta_r_first <- rnorm(n = max_freq, 
                       mean = 0, 
                       sd = 1
  ) *(sigma_signal^2)* sqrt(0.5 * spec_dens(1:max_freq))
  
  xi_r_first <- rnorm(n = max_freq, 
                      mean = 0, 
                      sd = sigma_0
  )
  
  alpha_r_first <- eta_r_first + xi_r_first
  
  eta_im_first <- rnorm(n = max_freq, 
                        mean = 0, 
                        sd = 1
  ) * (sigma_signal^2)*sqrt(0.5 * spec_dens(1:max_freq))
  
  xi_im_first <- rnorm(n = max_freq, 
                       mean = 0, 
                       sd = sigma_0
  )
  
  alpha_im_first <- eta_im_first + xi_im_first
  
  eta_mid <- rnorm(n = 1, 
                   mean = 0, 
                   sd = (sigma_signal^2)*sqrt(0.5 * spec_dens(n/2))
  )
  
  xi_mid <- rnorm(n = 1, 
                  mean = 0, 
                  sd = sigma_0
  )
  
  alpha_mid <- eta_mid + xi_mid
  
  ## Use conjugate symmetry to get remaining frequencies
  alph <- c(
    alpha_0,
    complex(real = alpha_r_first,
            imaginary = alpha_im_first),
    alpha_mid,
    rev(Conj(complex(real = alpha_r_first,
                     imaginary = alpha_im_first))
    )
  )
  
  ## Extract Signal
  eta <- c(
    eta_r_0,
    complex(real = eta_r_first,
            imaginary = eta_im_first),
    eta_mid,
    rev(Conj(complex(real = eta_r_first,
                     imaginary = eta_im_first))
    )
  )
  signal <- fft(eta, inverse = T) %>% Re()
  
  ## Extract Noise
  xi <- c(
    xi_r_0,
    complex(real = xi_r_first,
            imaginary = xi_im_first),
    xi_mid,
    rev(Conj(complex(real = xi_r_first,
                     imaginary = xi_im_first))
    )
  )
  
  white_noise <- fft(xi, inverse = T) %>% Re()
  
  ## FILTER TO TRANSFORM TO MA NOISE
  
  if(length(ma_coefs) == 0) {
    noise <- white_noise
  } else {
    noise <- stats::filter(
      white_noise,
      ma_coefs,
      method = "convolution",
      sides = 1,
      circular = T
    )
  }
  
  res <- data.frame(
    signal = signal,
    noise = noise,
    y = signal + noise,
    x = 1:n
  )
  
  return(list(
    res = res,
    order = length(ma_coefs)
  )
  )
  
}