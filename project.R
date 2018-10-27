# Project analysis
# Load the data
data <- read.csv("D:/Google Drive/yanhong/DePaul/CSC424/Project/kddcup.data/kddcup.data.corrected")

labels <- c("duration", "protocol_type","service","flag","src_bytes","dst_bytes","land","wrong_fragment",
                 "urgent","hot","num_failed_logins","logged_in","num_compromised","root_shell","su_attempted",
                 "num_root","num_file_creations","num_shells","num_access_files","num_outbound_cmds","is_host_login",
                 "is_guest_login","count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate",
                 "same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count",
                 "dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate",
                 "dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate","dst_host_rerror_rate",
                 "dst_host_srv_rerror_rate","classification")

names(data) <- labels

length(which(data$classification == "normal."))
# 972780
length(which(data$classification != "normal."))
# 3925650
# Check for missing data
sum(is.na(data))/prod(dim(data))
# 0 which indicates no missing data
colMeans(is.na(data))
# also 0 confirming it for all columns
# data set is known to have duplicates, let's remove them since they will effect the model
data.dedupe <- data[!duplicated(data),]
# Recheck normal/abnormal counts
length(which(data.dedupe$classification == "normal."))
# 812813
length(which(data.dedupe$classification != "normal."))
# 262178
# recode the response variable into a binary where 0 is normal and 1 is abnormal
data.dedupe$binClass <- ifelse(data.dedupe$classification == "normal.",0,1)
# Need numeric only for correlation plot
reddata <- data.dedupe[lapply(data.dedupe,class)!="factor"]
# Need numerics only with SD > 0 for correlation plot
reddata.delnovar <- reddata[lapply(reddata,sd)!= 0]

#################
# Feed into H2O
#################
library(h2o)
h2o.init()                                                                                                               

# Import data into H2O
data.hex <- as.h2o(reddata.delnovar)

summary(data.hex)
quantile(x=data.hex)

data.split <- h2o.splitFrame(data=data.hex,ratios=.75)
data.train <- data.split[[1]]
data.test <- data.split[[2]]

## Convert H2OFrames into R data.frame
train.df <- as.data.frame(data.train)
test.df <- as.data.frame(data.test)

#################
# EDA
#################

# Distribution of the response variable before de-duplication
barplot(table(data$classification), main="Frequency of connection classifications full data", 
        xlab="classification")
# Distribution of the response variable after de-duplication
barplot(table(data.dedupe$classification), main="Frequency of connection classifications de-duplicated data", 
        xlab="classification")
# Distribution of recoded response variable
hist(reddata.delnovar$binClass, main="Frequency of binary connection classifications", 
     xlab="classification")

# Check the correlation plot
library(corrplot)
c = cor(reddata.delnovar)
corrplot(c, method="square")
# some signs of strong correlations off of the main diagonal so risk of multicolinearity
# also some factors are strongly correlated with the resposne variable

#########################################################################
# Try PCA
#########################################################################
# PCA_Plot functions
#########################################################################

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

##########################
# Modeling in R
##########################
library("car")

# try building a simple logistic regression model
logit <- glm(binClass ~ ., data=train.df, family="binomial")
summary(logit)
vif(logit)
# Try to predict the class on test data
p.logit = predict(logit, newdata=test.df[,1:37], type="response")
p.logit.round <- round(p.logit)

# Compare the results of the prediction
table(p.logit.round, test.df$binClass)
confusionMatrix(as.factor(p.logit.round),reference = as.factor(test.df$binClass))

# Subset selection using stepwise
logit.step <- step(logit)
steplogit.formula <- formula(logit.step)
logit.step.model <- glm(steplogit.formula, data=train.df, family="binomial")
summary(logit.step.model)
vif(logit.step.model)
# Try to predict the class on test data
p.step = predict(logit.step.model, newdata=test.df[,1:37], type="response")
p.step.round <- round(p.step)

# Compare the results of the prediction
table(p.step.round, test.df$binClass)
confusionMatrix(as.factor(p.step.round),reference = as.factor(test.df$binClass))


# how many PCA components
dataPCA = prcomp(reddata.delnovar)
summary(dataPCA)
round(dataPCA$rotation, 2)
plot(dataPCA)
PCA_Plot(dataPCA)
loadings(dataPCA)

dataPCA2 = prcomp(reddata.delnovar, scale=T)
summary(dataPCA2)
round(dataPCA2$rotation, 2)
plot(dataPCA2)
PCA_Plot(dataPCA2)

# PCA
scoresData <- dataPCA2$x
library("ggfortify")
autoplot(prcomp(reddata.delnovar), data = reddata.delnovar, label=F, label.size=3, loadings=T, loadings.label=T, 
         loadings.label.size=3)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(reddata.delnovar, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
#biplot(fit)

## Logistic regression based on PCA reduced variables from first two components
logit.reduced <- glm(binClass ~ logged_in + count + serror_rate + srv_serror_rate + rerror_rate + srv_rerror_rate +
                       same_srv_rate + diff_srv_rate + dst_host_count + dst_host_srv_count + dst_host_same_srv_rate +
                       dst_host_srv_diff_host_rate + dst_host_serror_rate + dst_host_srv_serror_rate + 
                       dst_host_rerror_rate + dst_host_srv_rerror_rate, data=train.df, family="binomial")
summary(logit.reduced)
vif(logit.reduced)
# Try to predict the class on test data
p.logit2 = predict(logit.reduced, newdata=test.df[,1:37], type="response")
p.logit.round2 <- round(p.logit2)

# Compare the results of the prediction
table(p.logit.round2, test.df$binClass)
confusionMatrix(as.factor(p.logit.round2),reference = as.factor(test.df$binClass))

## Logistic regression based on PCA reduced variables from first two components
## Three features still showed up with high VIF manually removing them and re-running the model
## High VIF features are rerror_rate, srv_rerror_rate, and dst_host_srv_rerror_rate
logit.reduced2 <- glm(binClass ~ logged_in + count + serror_rate +  
                       same_srv_rate + diff_srv_rate + dst_host_count + dst_host_srv_count + dst_host_same_srv_rate +
                       dst_host_srv_diff_host_rate + dst_host_serror_rate + dst_host_srv_serror_rate + 
                       dst_host_rerror_rate, data=train.df, family="binomial")
summary(logit.reduced2)
vif(logit.reduced2)
# Try to predict the class on test data
p.logit3 = predict(logit.reduced2, newdata=test.df[,1:37], type="response")
p.logit.round3 <- round(p.logit3)

# Compare the results of the prediction
table(p.logit.round3, test.df$binClass)
confusionMatrix(as.factor(p.logit.round3),reference = as.factor(test.df$binClass))


## LASSO regression with glmnet
library(glmnet)
lassoglmmod <- glmnet(lasso.train <- sapply(train.df[,-38], as.numeric), y=as.factor(train.df$binClass), alpha=1, family="binomial")
plot(lassoglmmod, xvar="lambda")
summary(lassoglmmod)
names(lassoglmmod)
print(lassoglmmod)
min(lassoglmmod$lambda)
which(lassoglmmod$lambda == min(lassoglmmod$lambda))
coef(lassoglmmod, s=4.036995e-05)
lasso.pred <- predict(lassoglmmod, lasso.test <- sapply(test.df[,-38], as.numeric), s=4.036995e-05, type="class")
p.glmnet.round <- round(as.numeric(lasso.pred))

# Compare the results of the prediction
table(p.glmnet.round, test.df$binClass)
confusionMatrix(as.factor(p.glmnet.round),reference = as.factor(test.df$binClass))


## linear discriminant analysis
library(caret)
library(MASS)
trainLDA = lda(binClass ~ ., data=train.df)
trainLDA
plot(trainLDA)

# Try to predict the class on test data
p.test = predict(trainLDA, newdata=test.df[,1:37])$class
p.test

# Compare the results of the prediction
table(p.test, test.df$binClass)
confusionMatrix(p.test,reference = as.factor(test.df$binClass))

##########################
# H2O part of the analysis
##########################

# Set predictor and response variables
Y <- "binClass"
X <- c("duration","src_bytes","dst_bytes","land","wrong_fragment","urgent","hot","num_failed_logins","logged_in",
       "num_compromised","root_shell","su_attempted","num_root","num_file_creations","num_shells","num_access_files",
       "is_host_login","is_guest_login","count","srv_count","serror_rate","srv_serror_rate","rerror_rate",
       "srv_rerror_rate","same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count",
       "dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate","dst_host_srv_diff_host_rate",
       "dst_host_serror_rate","dst_host_srv_serror_rate","dst_host_rerror_rate","dst_host_srv_rerror_rate")

# h2o k-means cluster
kmeans <- h2o.kmeans(training_frame = data.train, k=3, x=1:4)

# h2o logistic regression
data.glm <- h2o.glm(training_frame = data.train, x=X,y=Y,family="binomial",alpha=.5)
summary(data.glm)

pred.glm <- h2o.predict(object=data.glm, newdata = data.test)
summary(pred.glm)

# h2o RandomForest
rf.mod <- h2o.randomForest(x=X,y=Y,data.train, validation_frame =data.test)
summary(rf.mod)
rf.mod@model$validation_metrics
h2o.hit_ratio_table(rf.mod,valid = T)[1,2]
rf.pred <- predict(rf.mod, data.test)
#h2o.confusionMatrix(rf.pred, data.test)
# Workaround for H2O confusion matrix
tmp <- round(as.data.frame(rf.pred))
p.rf.round <- as.numeric(unlist(tmp))
# Compare the results of the prediction
table(p.rf.round, test.df$binClass)
confusionMatrix(as.factor(p.rf.round),reference = as.factor(test.df$binClass))
