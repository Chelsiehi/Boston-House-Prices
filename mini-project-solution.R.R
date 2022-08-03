library(UsingR)
library(reshape)
library(ggplot2)
library(DataExplorer)
library(skimr)
library("ggpubr")
setwd("/Users/estherji") 

#Part 1
data <- read.csv("boston.csv", header=TRUE)
head(data)

#Validation
any(is.na(data))



#correlation 
plot_correlation(data)



#drop less relevant factors
data = subset(data, select = c(LSTAT, RM,PTRATIO, INDUS, TAX,MEDV) )


#boxplot(data)
meltData <- melt(data)
p <- ggplot(meltData, aes(factor(variable), value)) 

p + geom_boxplot() + facet_wrap(~variable, scale="free")


#median(data$MEDV)

a <- as.numeric(as.matrix(data$MEDV))

#table(a)

hist(a, xlab = "Boston housing price", main="Boston housing price distribution")
skim(data)
pairs(data)


#correlation 
plot_correlation(data)
#since cor(RM,LSTAT) = 0.61, REMOVE RM
data = subset(data, select = c(LSTAT, RM,PTRATIO, INDUS, TAX, MEDV) )
par(mfrow = c(1,1))
#######
#remove medv outliers
boxplot(data$MEDV, plot=FALSE)$out
outliers <- boxplot(data$MEDV, plot=FALSE)$out
x<-data
x<- x[-which(x$MEDV %in% outliers),]
boxplot(x)



#highest cor : RM 0.7, ZN 0.36, B 0.33
#LOWEST -0.74 LSTAT, -0.51 PARATIO, -0.48 INDUX， -0.48 tax， -0.43 nox, -0.39

#8 fictor 1. -0.74 LSTAT, 2. RM 0.7, 3. -0.51 PARATIO, 4. -0.48 INDUX，5. -0.47 tax， 6.-0.43 nox, 7. -0.39 CRIM, 8. -0.38 AGE, RAD


#2.	Sample linear regression with the highest correlated factor. (LSTAT)
slm <- lm(data$MEDV~data$LSTAT) 
ggscatter(data,"MEDV", "LSTAT", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% lower status of the population", ylab = "Housing Price")

lm(data$MEDV~data$LSTAT) 
#B1 is -0.95, this means the slope of this line is -0.95. Since -0.95 is a negative number, therefore, as % lower status of the population increases, 
#Housing Price will decrease. 
#B0 is 34.55, this means the when x equals to 0, the intersection of this line and the y-axis. 
#this also means when % lower status of the population is 0, the Housing Price is 34.55. 

#summary(slm)
#confint (slm)

#h0: there is no linear association.
#h1: there is a linear association.
#df = 506-2 = 504
#Get the critical f-value
qf(0.9, df1 = 1, df2 = 504)
#2.715517 with a = 0.1
anova(slm)
#F = 601.62
#Since 601.62 > 2.715517, reject null hypothesis.

#, calculate the  r-squared value and interpret this.  Also, calculate and interpret the 90% confidence interval

summary(slm)
#r_squared is 0.5441. Since r-squared is between 0 to 1, and 1 means this model that explains all the variation in the response variable around its mean. 
# Then, 0.5441 means 54.4% of the data fit in the model.

#90% confidence interval for LSATA
confint(slm, level = 0.9)
#The 90% confidence interval means there are 90% sure that the value should between the upper and lower limits. 
#Therefore, there are we are 90% sure that as x increased by 1, y will decrease -1.013877 to -0.8862212.

#– generates the predicted values (fitted values) for each sample data item used in the model
fitted(slm) 
#– generates the residual values for each sample data item
resid(slm)

# Residual Plots ( to assess linearity and constant variance
#abline(slm)
plot(fitted(slm),resid(slm),axes = TRUE , frame.plot = TRUE , xlab = "fitted values", ylab = "Residua")

plot(data$LSTAT, resid(slm) , axes = TRUE , frame.plot = TRUE , xlab = "LSTAT", ylab = "Residual")

# histogram of the residuals
hist(residuals(slm))
#Checking normality and homoscedasticity
plot(slm,2)
plot(slm,3)




################
influence(slm)
# Checking for influence points 
# Cooks distance
cooks.dist <- cooks.distance(slm)
cooks.distance(slm)
# Where the cook's distance is higher than 4/(n-k-1) (k is the number if parameters in equation)
which(cooks.dist > (4/(nrow(data)-2-1)))
#######mlr1
mlr1 <- lm(data$MEDV~data$LSTAT+data$PTRATIO+data$INDUS) 
anova(mlr1)
summary(mlr1)
plot(fitted(mlr1),resid(mlr1),axes = TRUE , frame.plot = TRUE , xlab = "fitted values", ylab = "Residua")

hist(residuals(mlr1))
plot(mlr1,2)
plot(mlr1,3)

#######mlr1 new
mlr1 <- lm(data$MEDV~data$LSTAT+data$PTRATIO) 
anova(mlr1)
summary(mlr1)
plot(fitted(mlr1),resid(mlr1),axes = TRUE , frame.plot = TRUE , xlab = "fitted values", ylab = "Residua")

hist(residuals(mlr1))
plot(mlr1,2)
plot(mlr1,3)

#######mlr1-log
lgmlr1 <- lm(log(data$MEDV)~log(data$LSTAT+data$PTRATIO)) 
lm(log(data$MEDV)~log(data$LSTAT+data$PTRATIO)) 
anova(lgmlr1)
summary(lgmlr1)
plot(fitted(lgmlr1),resid(lgmlr1),axes = TRUE , frame.plot = TRUE , xlab = "fitted values", ylab = "Residua")

hist(residuals(lgmlr1))
plot(lgmlr1,2)
plot(lgmlr1,3)



#######mlr2
mlr2 <- lm(data$MEDV~data$RM+data$PTRATIO+data$TAX) 
anova(mlr2)
plot(fitted(mlr2),resid(mlr2),axes = TRUE , frame.plot = TRUE , xlab = "fitted values", ylab = "Residua")

hist(residuals(mlr2))
plot(mlr2,2)
plot(mlr2,3)
#######mlr2-log
mlr2 <- lm(log(data$MEDV)~log(data$RM+data$PTRATIO+data$TAX)) 
anova(mlr2)
summary(mlr2)
plot(fitted(mlr2),resid(mlr2),axes = TRUE , frame.plot = TRUE , xlab = "fitted values", ylab = "Residua")

hist(residuals(mlr2))
plot(mlr2,2)
plot(mlr2,3)
