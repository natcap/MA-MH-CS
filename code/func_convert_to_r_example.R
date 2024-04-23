
source('./code/func_convert_to_r.R')
library(dplyr)


#  Data ==================================================================================

## data 1
head(mtcars, 5)
dat <- mtcars %>%
  dplyr::rename(y = mpg, x1 = hp, x2 = disp)

## data 2
library(car)
# summary(Mroz)
dat <- Mroz %>%
  dplyr::rename(y = inc, x2 = lwg, x1 = age) %>%
  dplyr::select(y, x1, x2)


# Models =================================================================================

## Pearson correlation ----
cor(dat$x1, dat$y)


## Simple linear regression (SLR) ----
lm1 <- lm(formula = y ~ x1, data = dat)
# summary(lm1)
# names(lm1)

r_squared <- summary(lm1)$r.squared
slope_coefficient <- coef(lm1)["x1"] %>% as.numeric(); #slope_coefficient
t_value <- summary(lm1)$coefficients['x1', "t value"]; #t_value

func_regression_R2_to_r(R2 = r_squared, slope_coef = slope_coefficient) ## good
func_regression_t_to_r(t = t_value, n = nrow(data_scaled))              ## good too!



## Multiple linear regression (MLR) ----
lm1 <- lm(formula = y ~ x1 + x2, data = dat)
summary(lm1)
names(lm1)

r_squared <- summary(lm1)$r.squared
slope_coefficient <- coef(lm1)["x1"] %>% as.numeric(); slope_coefficient
t_value <- summary(lm1)$coefficients['x1', "t value"]; t_value

func_regression_R2_to_r(R2 = r_squared, slope_coef = slope_coefficient) ## NOT good!
func_regression_t_to_r(t = t_value, n = nrow(data_scaled))              ## good






## SLR - Standardized ----
library(scales)
data_scaled <- as.data.frame(lapply(dat, scale))  # Standardize all variables
lm2 <- lm(y ~ x1, data=data_scaled)

# View the summary to see standardized coefficients
summary(lm2)
r_squared <- summary(lm2)$r.squared
slope_coefficient <- coef(lm2)["x1"] %>% as.numeric(); slope_coefficient
t_value <- summary(lm2)$coefficients['x1', "t value"]; t_value

func_regression_R2_to_r(R2 = r_squared, slope_coef = slope_coefficient) ## good
func_regression_t_to_r(t = t_value, n = nrow(data_scaled))              ## good


## MLR - Standardized ----
lm2 <- lm(y ~ x1+x2, data=data_scaled)

# View the summary to see standardized coefficients
summary(lm2)
r_squared <- summary(lm2)$r.squared
slope_coefficient <- coef(lm2)["x1"] %>% as.numeric(); slope_coefficient
t_value <- summary(lm2)$coefficients['x1', "t value"]; t_value

func_regression_R2_to_r(R2 = r_squared, slope_coef = slope_coefficient) ## not good!
func_regression_t_to_r(t = t_value, n = nrow(data_scaled))



