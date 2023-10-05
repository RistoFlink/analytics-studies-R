rm(list=ls())

library(datasets)
library(ggplot2)
library(dplyr)
library(corrplot)
install.packages("tsoutliers")
library(tsoutliers)


#let's load our first simple data set
icecream = read.csv("IceCream.csv", header = TRUE, sep = ";")
str(icecream)

#1. Linear dependence of explanatory variables
# - SKIP - only 1 explanatory variable in this data set

#our first model

model1 = lm(Sales_Weekly~Avg_Temp, data = icecream) #+ 0 would remove the intercept but for this course we will not do that
summary(icecream)
summary(model1)

ggplot(icecream, aes(x = Avg_Temp, y = Sales_Weekly)) +
  geom_point() +
  geom_line(aes(x = Avg_Temp, y = fitted.values(model1)), col = "red")

#2. Mean of the residuals
mean(residuals(model1))

#3. Homoskedasticity & 4. Residuals linearly independent
plot(fitted.values(model1), residuals(model1), type = "p", col = "blue", ylim = c(-200,200), pch = 16, ylab = "Residuals", main = "Residuals over time")
abline(a = 3 * sd(residuals(model1)), b = 0, col = "red", lty = 2)
abline(a = -3 * sd(residuals(model1)), b = 0, col = "red", lty = 2)
abline(a = 0, b = 0, col ="black", lty = 2)

#5. Correlation between the residuals and the independent (explanatory) variables
cor(residuals(model1), icecream$Avg_Temp)


#6. Check for Normal distribution of residuals
JarqueBera.test(residuals(model1))

#Example 2
icecream = read.csv("IceCream_2.csv", header = TRUE, sep = ";")
str(icecream)

#mtcars data set
data(mtcars)

depvar = mtcars$mpg
exvars = mtcars[, 2:11]

corrplot(cor(exvars), "number")

cormat = abs(cor(exvars))
diag(cormat) = 0
corrplot(cormat, "number")
while(max(cormat) >= 0.8) {
  #find explanatory variables with highest absolute correlation
  maxvar = which(cormat == max(cormat), arr.ind = TRUE)
  
  #select variable with the highest average correlation
  maxavg = which.max(rowMeans(cormat[maxvar[,1],]))
  
  #FYI
  print(rownames(maxvar)[maxvar[,1] == maxvar[maxavg, 1]])
  
  #removal
  exvars = exvars[,-maxvar[maxavg,1]]
  cormat = cormat[-maxvar[maxavg,1], -maxvar[maxavg,1]]
        
}

#let's make our model
my_data = cbind("mpg" = depvar, exvars)

linRegModel = lm(mpg~hp + drat + wt + qsec + vs + am + gear + carb, data = my_data)    
summary(linRegModel)      
      
linRegModel = lm(mpg~hp + drat + wt + qsec + am + gear + carb, data = my_data)    
summary(linRegModel)        
      
linRegModel = lm(mpg~hp + wt + qsec + vs + am + gear + carb, data = my_data)    
summary(linRegModel)        

linRegModel = lm(mpg~hp + wt + qsec + am + gear + carb, data = my_data)    
summary(linRegModel)         

linRegModel = lm(mpg~hp + wt + qsec + am + carb, data = my_data)    
summary(linRegModel)       
      
linRegModel = lm(mpg~wt + qsec + am, data = my_data)    
summary(linRegModel)       



