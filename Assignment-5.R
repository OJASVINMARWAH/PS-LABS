# Continuous Uniform Distribution
# punif : Cumulative distribution function
# punif : P(X <= k)
# 1 - punif : P(X > k)

# Discrete Uniform Distribution
# dunif : Probability density function
# dunif : P(X = k)

cat("P(X > 45) = ", 1 - punif(45, min = 0, max = 60), "\n")
cat("P(X <= 45) = ", punif(45, min = 0, max = 60), "\n")

p <- punif(30, 0, 60) - punif(20, 0, 60)
cat("P(20 < X < 30) = ", p, "\n")

# q2: Exponential Distribution
beta <- 1/0.5
# probability density function
#part 1
cat("P(x = 3) = ", dexp(3, rate = 0.5), "\n");

#part 2
points <- seq(0, 5, by = 1)       # Smaller steps for smooth curve
for (i in points){
  cat("P(X = ", i, ") = ", dexp(i, rate = 0.5), "\n")
}
plot(points, dexp(points, rate = 0.5), type = "h", main = "Exponential PDF", xlab = "x", ylab = "Density")


#part 3
cat(pexp(3, rate = 0.5), "\n")
plot(points, pexp(points, rate = 0.5), type = "h", main = "Exponential CDF", xlab = "x", ylab = "Cumulative Probability")

set.seed(1)
sam = rexp(1000, rate = 0.5)
hist(sam)
plot(sam,type="l")


# q3: Gamma Distribution
alpha<-2
beta <- 1/3
rate<-3

#part 1
cat("P(X = 3) = ", dgamma(3, shape = alpha, rate = rate), "\n")
#part 2
cat("P(X>=1)", 1 - pgamma(1, shape = alpha, rate = rate), "\n")

#part 3
cat("70th percentile : ", qgamma(0.7, shape = alpha, rate = rate), "\n")
