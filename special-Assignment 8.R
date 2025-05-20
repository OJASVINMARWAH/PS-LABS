# Poisson Distribution
# Poisson distribution is a discrete probability distribution that expresses the probability of a given number of events occurring in a fixed interval of time or space, given that these events occur with a known constant mean rate and independently of the time since the last event.
set.seed(123)

lam <- 4

sample = rpois(50, lam)
sample

theoretical_mean <- lam;
empirical_mean <- mean(sample)
variance <- var(sample)

cat("Theoretical Mean: ", theoretical_mean, "\n")
cat("Empirical Mean: ", empirical_mean, "\n")
cat("Variance: ", variance, "\n")

theoretical_variance <- lam
cat("Theoretical Variance: ", theoretical_variance, "\n")

# How does the distribution of sample means change as λ increases
lambda_values <- c(1, 5, 10, 20)
sample_means <- sapply(lambda_values, function(lam) {
  sample = rpois(1000, lam)
  mean(sample)
})
# NO USER DEFINED FUNCTION
cat("Sample Means for different λ values: ", sample_means, "\n")


#q2: Exponential distribution
lam <- 1.5
n =10
# check normality
sample = rexp(n, lam)
sample
# check normality
shapiro.test(sample)

