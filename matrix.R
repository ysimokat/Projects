
#Matrices
Z = matrix(c(1,4,1,6,1,-3,1,2),nrow = 4,ncol = 2, byrow = T)
Z
Y = matrix(c(-1,1,7,8),nrow = 4,ncol = 1)
Y
M = matrix(c(1,5,0,30,50,15,0,20,10),nrow = 3,ncol = 3,byrow = T)
M
N = matrix(c(-20,-5,0,0,20,5,20,10,20),nrow = 3,ncol = 3,byrow = T)
N
v = matrix(c(-2,2,6),nrow = 3,ncol = 1)
v
w = matrix(c(3,-7,5),nrow = 3,ncol = 1)
w

a = t(v) %*% w
a


b = (-3)*w
b

c = M %*% v
c

d = M + N
d

e = M - N
e

f = t(Z) %*% Z
f

g = solve(t(Z) %*% Z)
g

h = t(Z) %*% Y
h

beta = solve(t(Z)%*%Z) %*% t(Z) %*% Y
beta

j = det(t(Z)%*%Z)
j


house <- read.csv("C:/Users/Yanhong Simokat/Desktop/csc424/week_1/housedata.csv")
names(house)
head(house)

summary(house)

# Check for missing data
sum(is.na(house))/prod(dim(house))
colMeans(is.na(house))

# subset house to drop the ID and date field
house_set <- house[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
names(house_set)
head(house_set)
summary(house_set)

# correlations
corr <- round(cor(house_set),2)
library(ggplot2)
#library(ggcorrplot)
#ggcorrplot(corr)
library("corrplot")
corrplot(corr, method = "number")

# Stepwise regression
library("MASS")
fit <- lm(price ~ ., data = house_set)
step <- stepAIC(fit, direction="both", trace=F)
step$anova
summary(step)

step.result <- lm (price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + 
                     view + condition + grade + yr_built + yr_renovated + zipcode + 
                     lat + long + sqft_living15, data = house_set)
summary(step.result)
plot(house_set$price, predict(step.result), xlab = "Observed price", ylab = "Predicted price", main = "Stepwise model", abline(a = 0, b = 1, col = "red"))
par(mfrow=c(2,2))
plot(step.result)
par(mfrow=c(1,1))

# Visualization - histogram with density curve
hist(house_set$price, prob=T, col="grey", xlab = "Home Price", main="House Prices")
lines(density(house_set$price), col="blue", lwd=2)