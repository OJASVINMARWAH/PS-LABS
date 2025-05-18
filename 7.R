
n = 4
k <- c(2, 3, 4)
p = 1/6

sum_prob = 0
for (i in 1:length(k)) {  # Loop through all k values
  sum_prob = sum_prob + dbinom_custom1(n, p, k[i])
}

print(sum_prob)

n = 7
p = 1/6

total_prob = 0
for (i in 6:7){
  total_prob = total_prob + dbinom_custom1(n,p,i)
}
total_prob

p = 0.75
#k>=1
pmf = 0.99
#p(k=0)= 0.01

factorial <- function(n){
  fact = 1;
  for (i in 1:n){
    fact = fact*i;
  }
  return (fact)
}

combination <- function(n,k){
  return (factorial(n)/(factorial(k)*factorial(n-k)))
}
dbinom_custom1 = function(n,p,k){
  return (combination(n,k)*(p^k)*((1-p)^(n-k)))
}

log(0.01)/log(3/4)


n = 5
p = 17/250
print(p) 
#binomail distibution
k = 3

dbinom_custom1(n,p,k)

dpois1 = function(lambda,k){
  return ((lambda^k)*exp(-lambda)/gamma(k+1))
}
lambda = 0.001 * 2000
1 - dpois1(lambda,1)-dpois1(lambda,0) - dpois1(lambda,2)


dpois1(5.2,0) + dpois1(5.2,1)


p  =0.05
n = 15

#k = 0,1

lambda = n*p
1 - dpois1(lambda, 0)-dpois1(lambda, 1)


n = 500
k = 1
p = 1/365

np = n*p
dpois1(np,k)


p = 0.02
n = 50
round(1 - dpois1(n*p,0) - dpois1(n*p,1),2)


lam = 3
k = 3

1 - dpois1(lam,0) -dpois1(lam,1) -dpois1(lam,2) 


f = function()