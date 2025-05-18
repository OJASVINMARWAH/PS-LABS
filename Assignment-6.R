# Assignment 6

#q1: CONTINUOUS PROBABILITY DISTRIBUTION
library(pracma)

f <- function(x,y){
  if (x>=0 & y>=0 & x<=1 & y<=1){
    return (2*(2*x + 3*y)/5)
  }
  else return (0)
}

f2 <- function(x,y){
  return (2*(2*x + 3*y)/5)
}

# check whether joint density function or not using integral2()
integral2(f2, 0, 1, 0, 1)


# marginal distribution of y
hy <- function(x){
  return (f2(x,0))
}

marginal_h <- integrate(hy,0,1)
marginal_h
# marginal distribution of x
gx <- function(y){
  return (f2(1,y))
}

marginal_g <- integrate(gx,0,1)
marginal_g

new_f = function(x,y){
  return (x*y*f2(x,y))
}

# expected value of g(x,y) = x*y
expected_value <- integral2(new_f,0,1,0,1)
expected_value

#q2: DISCRETE PROBABILITY DISTRIBUTION

# joint mass function in matrix form
pmf <- function(x,y){
  return ((x+y)/30)
}

x = c(0:3)
y = c(0:2)

mat1 <- matrix(c(pmf(x[1],y[1]), pmf(x[1],y[2]), pmf(x[1],y[3]),
                 pmf(x[2],y[1]), pmf(x[2],y[2]), pmf(x[2],y[3]),
                 pmf(x[3],y[1]), pmf(x[3],y[2]), pmf(x[3],y[3]),
                 pmf(x[4],y[1]), pmf(x[4],y[2]), pmf(x[4],y[3])),
               nrow=4, ncol=3, byrow=TRUE)
mat1
# check whether joint mass function or not using sum()
sum(mat1)

# marginal distribution of x using apply()
marginal_x <- function(x){
  return (apply(mat1, 1, sum))
}

marginal_x(x)

# marginal distribution of y using apply()
marginal_y <- function(y){
  return (apply(mat1, 2, sum))
}
marginal_y(y)

# CONDITIONAL DISTRIBUTION at x = 0 given y = 1
conditional_distribution <- function(x,y){
  return (pmf(x,y)/marginal_y(y))
}
conditional_distribution(0,1)


