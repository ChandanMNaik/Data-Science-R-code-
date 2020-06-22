## Loading the data sets ##

datasets::longley
Long <- datasets::longley

## Exploring the data set first 10 values ##

head(Long)
head(Long,10)

## Exploring the data set's last 10 values ##

tail(Long)
tail(Long,10)

## Plotting the various values ##

plot(Long)
plot(Long$GNP.deflator, col = 'red')
plot(Long$GNP, col = 'blue')
plot(Long$Unemployed, col = 'darkGreen')

## Bar Plot ##

barplot(Long$Armed.Forces, col = 'purple')
barplot(Long$Population, col = 'green') 
barplot(Long$Employed, col = 'blue')

## Histogram Plot ##

hist(Long$GNP, col = 'red')
hist(Long$Armed.Forces, col = 'yellow')

## Comparission between variables ##

plot(Long$GNP.deflator,Long$Unemployed, main = 'GNP.deflator vs Unemployed',
     xlab = 'GNP.deflator', ylab = 'Unemployed', col ='red')

plot(Long$Armed.Forces, Long$GNP, main = 'Armed.Forces vs GNP',
     xlab = 'Armed.Forces', ylab = 'GNP', col = 'green')

## Comparission between variables by barplot ##

barplot(Long$Employed,Long$Unemployed, main = 'Employed vs Unemployed',
     xlab = 'Employed', ylab = 'Unemployed', col = 'yellow')

barplot(Long$GNP,Long$Armed.Forces, main = 'GNP vs Armed.Forces',
        xlab = 'GNP', ylab = 'Armed.Forces', col = 'blue')

## Finding the outliers ##

boxplot(Long$GNP.deflator)
boxplot(Long$GNP)
boxplot(Long$Unemployed)
boxplot(Long$Armed.Forces)
boxplot(Long$Population)

## Finding the maximum values ##

max(Long$GNP.deflator)
max(Long$Employed)
max(Long$Unemployed)

## Linear Regression between GP & Unemployed ##

model1 <- lm(Unemployed~GNP, data = Long)
summary(model1)

## Linear Regression between Employed & GNP.deflator ##

model2 <- lm(Employed~GNP.deflator,data = Long)
summary(model2)

## Predictions of Models ##

pred <- predict(model1)
pred

pred <- predict(model2)
pred

## Mltiple Regression ##

model3 <- lm(GNP.deflator~GNP+Unemployed+Armed.Forces+Population+Employed, data = Long)
summary(model3)

## Prediction for model3 ##

pred <- predict(model3)
pred

## Regression Plotting ##

plot(model1)
avPlots(model1)

plot(model2)
avPlots(model2)
influenceIndexPlot(model2)

plot(model3)
avPlots(model3)
influenceIndexPlot(model3)

## Prediction irrespective of Unemployed ##

predict_Employed <- predict.lm(model)
predict_Employed

## Checking Colinearity ##

data1 <- data.frame(Long$GNP.deflator,Long$GNP,Long$Unemployed,Long$Armed.Forces,Long$Population,Long$Year,Long$Employed)
cor(data1)

