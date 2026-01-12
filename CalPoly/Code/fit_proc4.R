## CODE THAT WRITES A FUNCTION THAT PERFORMS
## OUR SPECTRAL FITTING PROCEDURE

fit_proc_ma_q <- function(x, 
                          y,
                          kernel_type = "matern",
                          ma_order = 2,
                          skip_orders = c(),
                          start_coef = c(100, 90, 1, 1),
                          ell = 0.1,
                          lin_trend = FALSE) {
  ## control optional arguments for trend
  if(lin_trend == FALSE){
    beta_1_hat <- 0
  } else {
    beta_1_hat <- lm(y ~ x)$coefficients[2]
  }
  
  ## extract maximum possible number of frequencies
  n <- length(x)
  ord <- as.integer(ma_order)
  if(n %% 2 != 0){
    B <- floor((n - 1)/2) + 1
  } else {
    B <- (n / 2)
  }
  
  ## extract Fourier coefficients
  alpha_k <- fft(y)/n
  b_k <- fft(x) / n
  
  ## obtain theta_k terms
  theta_k <- (0.5) * Mod(alpha_k - beta_1_hat * b_k)^2
  theta_k_0 <- theta_k[1]
  theta_k <- theta_k[2:B]
  
  ## fit
  if(kernel_type == "matern") {
    S_k_full <- (16 * sqrt(5) / 15) * ell * 
      (1 + (4*pi^2*ell^2 / 5) * (2:B)^2)^(-3)
  } else if(kernel_type == "sq-exp") {
    S_k_full <- sqrt(2*pi*ell^2) * 
      exp(-2 * pi^2 * ell^2 * (2:B)^2)
  }
  
  base_glm <- glm(theta_k ~ 1 + S_k_full,
                  family = Gamma(link = "identity"))
  
  ## generate q cosine covariates
  if(ord > 0) {
    if(length(skip_orders) == 0) {
      index_set <- 1:ord
    } else{
      index_set <- (1:ord)[-skip_orders]
    }
    cos_names <- paste0("cos", index_set)
    for(j in index_set) {
      assign(cos_names[which(index_set == j)], cos(j * 2 * pi * (2:B) / n))
    }
    rhs_name <- reformulate(c("S_k_full", paste0("cos", index_set)))
    glm_gamma_ma_q <- update(base_glm, formula = rhs_name,
                             start = start_coef)
    
  } else if(ord == 0){
    glm_gamma_ma_q <- base_glm
  }
  
  theta_k_hat_ma_q <- predict(glm_gamma_ma_q, type="response")
  
  v_k_hat_ma_q <- theta_k_hat_ma_q - 
    coef(glm_gamma_ma_q)[1]
  
  if(ord > 0){
    if(length(skip_orders) == 0) {
      index_set2 <- 3:(ord + 2)
      for(j in index_set2){
        v_k_hat_ma_q <- v_k_hat_ma_q - 
          coef(glm_gamma_ma_q)[j] * eval(parse(text = paste0("cos", j - 2)))
      }
    } else {
      index_set2 <- (3:(ord + 2))[-skip_orders]
      for(j in index_set2){
        v_k_hat_ma_q <- v_k_hat_ma_q - 
          coef(glm_gamma_ma_q)[which(index_set2 == j)] * eval(parse(text = paste0("cos", j - 2)))
      }
    }
  }
  
  weights_ma <- (v_k_hat_ma_q / theta_k_hat_ma_q)
  
  if(n %% 2 == 0){
    weighted_spectrum_new <- (alpha_k - beta_1_hat * b_k) * c(
      1,
      weights_ma,
      0,
      rev(weights_ma)
    )
  } else {
    weighted_spectrum_new <- (alpha_k - beta_1_hat * b_k) * c(
      1,
      weights_ma,
      rev(weights_ma)
    )
  }
  
  fitted_residuals <- Re(fft(weighted_spectrum_new, inverse = TRUE))
  fitted_trend <- beta_1_hat * x
  
  fitted_vals_new <- fitted_residuals + fitted_trend
  
  res <- list(
    fitted = fitted_vals_new,
    coefs = coef(glm_gamma_ma_q),
    dev = glm_gamma_ma_q$deviance,
    fitted_thetas = c(NA, glm_gamma_ma_q$fitted.values),
    theta_display = c(theta_k_0, 
                      theta_k)
  )
  
  return(res)
}
