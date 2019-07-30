# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# (a) For the random sample (10,27,30,40,46,51,52,104,146), calculate the two-sided
# confidence interval based on the quantiles of the standard normal distribution

# Normally if n is smaller than 30, you would use t-distribution, but here
# I was given to use NORMAL DISTRIBUTION, so I will use it.
# ( X bar +- (Z * Margin of Error) ) 
sample_1 = c(10, 27, 30, 40, 46, 51, 52, 104, 146)
sample_1_mean = round(mean(sample_1), 2)
# Standard error
se_1 = round(sd(sample_1) / sqrt(length(sample_1)), 2)
z = 1.96

# two sided 95% Confidence interval of NORMAL DISTRIBUTION
z_lower_bound_sample_1 = round(sample_1_mean - (z * se_1), 2)
z_upper_bound_sample_1 = round(sample_1_mean + (z * se_1), 2)


# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# (b) For the random sample (10,27,30,40,46,51,52,104,146), calculate the two-sided
# confidence interval based on the quantiles of the T-DISTRIBUTION.
# ( X bar +- (t * Margin of Error) ) 

# df = n-1
n = length(sample_1)
df = n - 1
t = 2.306

# two sided 95% Confidence interval of T DISTRIBUTION
t_lower_bound_sample_1 = round(sample_1_mean - (t * se_1), 2)
t_upper_bound_sample_1 = round(sample_1_mean + (t * se_1), 2)


# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# (C) Write your own R-function, which receives as input a vector of the sample
# as well as the confidence interval 1- alpha returns as output both computed
# confidence intervals
conf.calculator <- function(sample, alpha) {
  sample.mean = round(mean(sample), 2)
  
  # Standard error
  se = round(sd(sample) / sqrt(length(sample)), 2)
  
  # calculate the corresponding z score to the siginificance levell
  z = qnorm(alpha/2)
  
  # two sided 95% Confidence interval of NORMAL DISTRIBUTION
  lower_bound_sample = round(sample.mean + (z * se), 2)
  upper_bound_sample = round(sample.mean -   (z * se), 2)
  
  conf_interval_sample = c(lower_bound_sample, upper_bound_sample)
  
  return(conf_interval_sample)
}

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# (D) In your function, complete the calculation of a confidence interval using bootstrapping
# To do this, use the R function sample () from your original sample of size n total B = 10000
# to generate new sample of the extent n with replacement and determine the corresponding 
# empirical quantile so that they are within limits of the two-sided confidence interval to the level 1-a

# I don't quite understand what this question says, so I will just follow normal bootstrapping

# generate a B=10000 times of bootstrap samples of our sample with replacement
set.seed(13579)
num.sample = length(sample_1)
B <- 10000
Boot.sample <- matrix( sample(sample_1, size=B*num.sample,
                              replace=TRUE), ncol=B, nrow=num.sample)

# Make a confidence interval of 1-a, I will do 95% two sided confidence interval here
# And show the confidence interval
Boot.Means <- colMeans(Boot.sample) 
Boot_lower_bound = quantile(Boot.Means, prob=0.025)
Boot_upper_bound = quantile(Boot.Means, prob=0.975)

# (E) Familiarize yourself with the R functions boot () and boot.ci() of the R-package boot 
# and use them to calculate confidence interval using bootstrapping
library(boot)

# I have a hard time understanding why I have to set the function first to use
# bootstrapping functions. These codes are below don't work, and I might need help.
meanfun <- function(data, i){
  d <- data
  return(mean(d))   
}

# boot(sample_1, meanfun, R=500)
# boot.ci(boot.out = boot, type = c("norm", "basic", "perc", "bca"))
