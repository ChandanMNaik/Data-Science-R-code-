## Lodaing the CSV file ##

data1 <- read.csv("F:/Data Science/R Code dataset/Cars.csv")
data1 <- Cars1

## Summary of dataset ##

summary(data1)
data1

## Scateer Plot ##

pairs(data1)

## Correlation Matrix ##

cor(data1)

## Plots ##

boxplot(data1$HP)
boxplot(data1$MPG)
boxplot(data1$VOL)
boxplot(data1$SP)

## Plot HP vs VOL ##

plot(data1$HP,data1$VOL, main = 'HP VS VOL', xlab = 'HP', ylab ='VOL', col = 'Blue')

## Plot MPG vs SP ##

plot(data1$MPG,data1$SP, main = 'MPG VS SP',
     xlab = 'MPG', ylab = 'SP', col = 'purple')

## Bar Plots ##

barplot(data1$WT,col = 'yellow')
barplot(data1$HP,col = 'blue')

## Bar plot MOG vs WT ##

barplot(data1$MPG,data1$WT, main = 'MPG VS WT',
        xlab = 'MPG', ylab = 'WT', col = 'green')

## Histogram Plots ##

hist(data1$HP, col = 'green')
hist(data1$VOL, col = 'yellow')

## Checking Null Values ##

sum(is.na(data1))

## Multiple Regression ##

model.car <- lm(MPG~HP+VOL+SP+WT, data = data1)
summary(model.car)

## Regression between MPG & other parameters ##

model.car <- lm(MPG~HP, data = data1)
summary(model.car)

model.car <- lm(MPG~VOL, data = data1)
summary(model.car)

model.car <- lm(MPG~SP, data = data1)
summary(model.car)

model.car <- lm(MPG~WT, data = data1)
summary(model.car)

## Regression MOdels & Summary ##

model.car <- lm(MPG~., data = data1)
summary(model.car)

## Omittiing Outliers ##

model.car <- lm(MPG~., data = data1[-79,])
summary(model.car)

##Influncer plot ##

influence(model.car)

## Removing outliers & non significant variable ##

model.car <- lm(MPG~., data = data1[-c(70,80,60,50,40),-c(3,5)])
summary(model.car)

## Re-checking Outliers ##

influence(model.car)
 
## Linear Regression  between HP & MPG ##

model.car1 <- lm(MPG~HP, data = data1)
summary(model.car1)

## Prediction ##

predict <- predict(model.car)
predict

## Comparing ##

plot(data1$MPG, type = 'l', col = 'green')
lines(predict, type =  'l', col = 'blue')
plot(predict, type = 'l', col = 'red')



