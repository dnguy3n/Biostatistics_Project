model <- reaction$Response.time

logmodel <- log(model)
hist(logmodel)
shapiro.test(logmodel)
qqnorm(logmodel)
qqline(logmodel)

hist(logmodel, main='Reaction times', xlab='log of Reaction time in seconds')

qqnorm(logmodel, ylab = 'Log of Reaction times')
qqline(logmodel)


table(reaction$Activity)
barplot(table(reaction$Activity), xlab = 'Activities', ylab='Frequency',main='Counts of activities')

hours <- reaction$Hours
hist(hours)
sqrt(hours)
sqrthours <- sqrt(hours)
hist(sqrthours)
loghours <- log(hours)
hist(loghours, xlab='Log of hours spent on activity', main='Hours Spent on Activities')
shapiro.test(loghours)
qqnorm(loghours)
qqline(loghours)
summary(loghours)

multivar_reaction <- lm(reaction$Response.time ~ reaction$Activity + loghours)

#Checking assumption for normal distribution of residuals
qqnorm(multivar_reaction$residuals)
qqline(multivar_reaction$residuals)

hist(multivar_reaction$residuals, xlab='Residuals', main='Histogram of Residual data of Response time for activity + hours')

#Checking assunmption for GLM for homogeneity of variance
plot(multivar_reaction$fitted.values, multivar_reaction$residuals, ylab = 'Residuals', xlab = 'Fitted Values', main = 'Equal Variance Assumption Test')
abline(h=0)

#Checking assumption for linear relation between numeric predictor and response variable
plot(loghours, reaction$Response.time, ylab='Response Time', xlab = 'Log of the hours spent on activities', main='Response time with respect to Log Hours')

summary(multivar_reaction)
summary(lm(reaction$Response.time ~ reaction$Activity * loghours))

install.packages('gplots')
library(gplots)

Sports <- reaction[reaction$Activity == 'Sports',]
Sedentary <- reaction[reaction$Activity =='Sedentary',]
Games <- reaction[reaction$Activity == 'Games',]

plot(log(Sports$Hours),Sports$Response.time, pch=17, col='red',cex=.8, xlab='Log of Hours spent on Activity', ylab = 'Response Time', main='Interaction of activities and log of time spent on activities')
points(log(Games$Hours),Games$Response.time, pch=17, col='blue',cex=.8)
points(log(Sedentary$Hours),Sedentary$Response.time, pch=17, col='orange',cex=.8)

abline(lm(Response.time~log(Hours), data=Sports), col='red')
abline(lm(Response.time~log(Hours), data=Games), col='blue')
abline(lm(Response.time~log(Hours), data=Sedentary), col='orange')
legend('topright', title='Activity', c('Sports', 'Games', 'Sedentary'), col=c('red','blue','orange'), pch=c(17,16),inset=0.01,cex=.8)

summary(Sports$Hours)
summary(Games$Hours)
summary(Sedentary$Hours)
