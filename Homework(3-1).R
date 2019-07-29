# Homework 3

# Illustrate the central limit theorem

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# (a) Simulate (large numbers, such as sim = 10000) independent samples of the
# extent n for different distributions1 and demonstrate the central limit theorem
# by plotting the empirical distributioin of the simulated arithmetic mean2
# and the corresponding normal distribution

# From what I understand, this question wants me to do,
# - (A-1)Simulate Normal Distribution of large number
# - (A-2)Extract the sample of N size for multiple times, let's say 10 times here
# - (A-3)Plot those means to the graph

# (A-1)
# I made a sequence of numbers between 150 and 200 increasing by 1/1000
# I will use it for normal distribution
# So my population is 50,000
x.norm <- seq(150, 200, by = .001)

# Choose the mean and sd for normal distribution and make normal distribuiton
norm.dist <- dnorm(x.norm, mean=175, sd = 5)

# Plot normal distribution
plot(x.norm, norm.dist, main='Normal Distribution')

# (A-2)Extract the sample of N size for multiple times, let's say n=30, and 10 times here

sample_means = 0

for(i in 1:10) {
  sample = rnorm(30 ,mean=175, sd=5)
  sample_mean = round(mean(sample), 2)
  sample_means[i] = sample_mean
}

# (A-3) Plot those means to the graph
plot(x.norm, norm.dist, main='Normal Distribution')
points(sample_means, y = rep(0, times=10), col="darkred")


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# (B) Using the simulation from (a) determine whether the rule of thumb that the arith
# metic mean is approximately normally distributed from n = 30 yield truly
# reasonable approximations even for a very skewed distribution (for example,
# lognormal distribution with standard deviation 2). How big should you choose n?

# Make a lognormal distribution that has Standard deviation of 2
x.log = rlnorm(n = 1000, meanlog = 0, sdlog = 2)
plot(density(x.log), main='lognormal distribution with sd 2')
# collect sample means that has n = 30 and plot them to check if it makes normal distribution
log_sample_means = 0

for(i in 1:1000) {
  log_sample_mean = mean(rlnorm(n = 1000, meanlog = 0, sdlog = 2))
  log_sample_means[i] = round(log_sample_mean, 2)
}

hist(log_sample_means, main="Sampling distribution of log sample means", xlab="log Sample Means", 
     prob=T, col="darkred")
lines(density(log_sample_means), col="blue", lwd=5)

# How big should you choose n? 
# Answer: The most books suggest that the rule of thumb is 30, and from the what I've just conducted,
# I assume 30 is enough and it makes normal distribution


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# (C) Examine the above rule of thumb for symmetric distributions

# I will extract sample size of 30 and calculate the sample mean
# Do it for 1,000 times to see if it makes normal distribution
sample_means = 0

for(i in 1:1000) {
  sample = rnorm(30 ,mean=175, sd=5)
  sample_mean = round(mean(sample), 2)
  sample_means[i] = sample_mean
}

hist(sample_means, main="Sampling distribution of sample means", xlab="Sample Means", 
     prob=T, col="brown")
lines(density(sample_means), col="blue", lwd=5)
