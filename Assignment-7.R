#Assignment 7

#q1 : t distribution
n = 100
df = n-1

t_sample <- rt(n, df) # generates sample of 100 numbers

hist(t_sample, main = "Histogram of t-distribution", xlab = "t values", ylab = "Frequency", col = "lightblue", border = "black")


#q2: Chi-square distribution
n = 100
chi2_df2 <- rchisq(n, df = 2)
chi2_df10 <- rchisq(n, df = 10)
chi2_df25 <- rchisq(n, df = 25)

hist(chi2_df2, main = "Histogram of Chi", xlab = "values", ylab = "Frequency", col = "lightblue", border = "black")

#q3: t distribution

x =  seq(-6,6,length=10)

x

df1 =  dt(x, df = 1)
df2 =  dt(x, df = 4)
df3 =  dt(x, df = 10)

plot(x, df3, type = "l", col = "red", lwd = 2, 
     main = "t-Distribution Density Comparison",
     xlab = "x", ylab = "Density")



