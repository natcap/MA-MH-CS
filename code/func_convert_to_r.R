
##' 
##' Convert R2 or other correlation coefficients to standard correlation coefficient
##' 
##' Ref: Muruga, P., A. C. Siqueira, and D. R. Bellwood. 2024. 
##'   Meta-analysis reveals weak associations between reef fishes and corals. 
##'   Nature Ecology & Evolution 8:676-685.
##'

## Spearman's rank correlation coefficient (ρ) -------------------------------------------

func_spearman_to_r <- function(x) {
  rho = x
  r = 2*sin(pi*rho/6)
  return(r)
}


## Kendall’s rank correlation coefficient (τ) --------------------------------------------
func_kendall_to_r <- function(x) {
  tau = x
  r = sin(pi*tau/2)
  return(r)
}

## For the coefficient of determination (R2), with regression slope (β)
func_regression_R2_to_r <- function(R2, slope_coef) {
  ##' 
  ##' Simple Linear Regression (SLR; with one predictor, or one independent variable)
  ##'   This function works the best when the model is simple linear regression 
  ##' Multiple Linear Regression (MLR; with two or more predictors)
  ##'   It may also works for MLR, but the R2 is not for one particular predictor, but for 
  ##'   all the predictors. So in the latter case, it might not be accurate. 
  ##'   However, if they report the `t-value` for each predictor, it would be better we use
  ##'   t-value to estimate `r`
  r = slope_coef*sqrt(R2)/abs(slope_coef)
  return(r)
}

func_regression_t_to_r <- function(t, n) {
  ##' `n` is the sample size; here we use `n-2` as the degrees of freedom 
  ##' 
  r = sqrt(t^2/(t^2 + n-2))*(t/abs(t))
  return(r)
}

## partial correlation coefficients
func_pcor_to_r <- function(r_yx1_x2, r_yx2_x1, R2) {
  #' r_yx1_x2: The partial correlation coefficient between y and x1 controlling for x2
  #' r_yx2_x1: The partial correlation coefficient between y and x2 controlling for x1
  #' R2: the R2 for y~x1+x2
  #' Aloe, A. M., & Thompson, C. G. (2013). The synthesis of partial effect sizes. 
  #'  Journal of the Society for Social Work and Research, 4(4), 390–405. 
  #'    https://doi.org/10.5243/jsswr.2013.24
  r = sqrt(
    (r_yx1_x2^2 - R2)/(r_yx1_x2^2 - 1)
  )
  return(r)
}

