
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

# Check the correlation plot
library(corrplot)
c = cor(reddata.delnovar)
corrplot(c, method="square")
# some signs of strong correlations off of the main diagonal so risk of multicolinearity
# also some factors are strongly correlated with the resposne variable


# try PCA
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


# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(reddata.delnovar, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Varimax Rotated Principal Components
# retaining 2 components 
library(psych)
varifit <- principal(reddata.delnovar, nfactors=2, rotate="varimax")
varifit # print results

# problem 3
problem3.data <- read.csv("D:/Google Drive/yanhong/DePaul/CSC424/HW3/canon.csv", header=T)

# Check for missing data
sum(is.na(problem3.data))/prod(dim(problem3.data))
colMeans(is.na(problem3.data))

# There are missing values reduce to complete cases
prob3.data.complete <- problem3.data[complete.cases(problem3.data),]

library(CCA)
library(yacca)
library(psych)

###################################################################
# This is a nice function for computing the Wilks lambdas for 
# CCA data from the CCA library's method
# It computes the wilkes lambas the degrees of freedom and te 
# p-values
###################################################################

ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

library(corrplot)

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

##########
# Let's check to see if two variable groups makes sense
##########

# Check the correlation plot
c = cor(prob3.data.complete)
corrplot(c, method="ellipse")

# how many PCA components
dataPCA = prcomp(prob3.data.complete)
summary(dataPCA)
round(dataPCA$rotation, 2)
plot(dataPCA)
PCA_Plot(dataPCA)
loadings(dataPCA)

dataPCA2 = prcomp(prob3.data.complete, scale=T)
summary(dataPCA2)
round(dataPCA2$rotation, 2)
plot(dataPCA2)
PCA_Plot(dataPCA2)


# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(prob3.data.complete, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Varimax Rotated Principal Components
# retaining 10 components 
library(psych)
varifit <- principal(prob3.data.complete, nfactors=10, rotate="varimax")
varifit # print results

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(prob3.data.complete, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:3] 
plot(load,type="n") # set up plot 
text(load,labels=names(prob3.data.complete),cex=.7) # add variable names

##########
# The analysis above does suggest two variable groupings is reasonable
##########

##########
# Split the data into attidunal and health variables
##########

attidunal <- prob3.data.complete[,c(6,7,8,10)]
health <- prob3.data.complete[,c(2,3,4,5,9)]

##########
# CCA
##########

# This gives us the cannonical correlates, but no significance tests
c = cancor(attidunal, health)
c

# The CCA library has more extensive functionality
library(CCA)

#Breakdown of the Correlations
matcor(attidunal, health)

#Correlations between attidunal and attidunal (X)
#Correlations between health and health (Y)
cc = cc(attidunal, health)
cc$cor

#Funcrions for CCA
ls(cc)

#XCoef Correlations
cc$xcoef

#YCoef Correlations
cc$ycoef

#Calculate Scores
loadings = comput(attidunal, health, cc)
ls(loadings)

#Correlation X Scores
loadings$corr.X.xscores

#Correlation Y Scores
loadings$corr.Y.yscores

#Wilk's Lambda Test
wilks = ccaWilks(attidunal, health, cc)
round(wilks, 2)

# Now, let's calcualte the standardized coefficients
s1 = diag(sqrt(diag(cov(attidunal))))
s1 %*% cc$xcoef

s2 = diag(sqrt(diag(cov(health))))
s2 %*% cc$ycoef

# A basic visualization of the cannonical correlation
plt.cc(cc)

################################################################
# Now, let's try it with yacca
################################################################

library(yacca)
c2 = cca(attidunal, health)
c2

#CV1
helio.plot(c2, cv=1, x.name="attidunal Values", 
           y.name="health Values")

#CV2
helio.plot(c2, cv=2, x.name="attidunal Values", 
           y.name="health Values")

#Function Names
ls(c2)

# Perform a chisquare test on C2
c2
ls(c2)
c2$chisq
c2$df
summary(c2)
round(pchisq(c2$chisq, c2$df, lower.tail=F), 3)

summary(c2, test="Wilks")




