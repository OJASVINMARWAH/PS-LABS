# Assignment 8

#q1 : CLT Distribution

# 1. Import data
data <- read.csv("C:/Users/Ojasvin/Downloads/Clt-data.csv")

# 2. Validate data : counting the number of rows
nrow(data)
#or 
cat("Dimension of the data frame:", dim(data), "\n")

#print first 10 rows
head(data, 10)

#3. Calculate population mean
population_mean <- mean(data$Wall.Thickness)
cat("Population mean:", population_mean, "\n")

# Calculate population standard deviation
population_sd <- sd(data$Wall.Thickness)
cat("Population standard deviation:", population_sd, "\n")

# 3 (b) Plot Histogram
hist(data, main = "Histogram of Population Data", xlab = "Values", ylab = "Frequency", col = "lightblue", border = "black")

# Add mean line
abline(v = population_mean, col = "red", lwd = 2)

#q1: phase 2 
# Create sampling distribution with 300 samples of size 10
sampling_distribution <- replicate(300, mean(sample(data$Wall.Thickness, 10, replace = TRUE)))

# Plot the sampling distribution
hist(sampling_distribution, 
     main = "Sampling Distribution of Sample Mean", 
     xlab = "Sample Mean", 
     ylab = "Frequency", 
     col = "lightgreen", 
     border = "black")

# q2
dataset <- data.frame(
  Age = c(58,69,43,39, 63, 52, 47, 31, 74, 36),
  Cholestrol = c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)
)

plot(dataset$Age, dataset$Cholestrol, 
     main = "Scatterplot of Age vs Cholestrol", 
     xlab = "Age", 
     ylab = "Cholestrol", 
     col = "blue")

# Add regression line
model <- lm(Cholestrol ~ Age, data = dataset)
new_data <- data.frame(Age =  60)
predicted_chol <- predict(model, new_data)
cat("Predicted Cholestrol for Age 60:", predicted_chol, "\n")

#q3

before <- c(145,173,158,141,167,159, 154, 167, 145, 153)
after <- c(139, 165, 152, 138, 162, 155, 150, 163, 141, 149)

differences <- after - before

t.test(after, before, 
       paired = TRUE,       
       alternative = "less", 
       conf.level = 0.95)
