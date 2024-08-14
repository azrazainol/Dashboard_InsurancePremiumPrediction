setwd("/Users/macbookair/Desktop/STQD6134 Business Analytics/project")
insurance <- read.csv("insurance.csv")
head(insurance)
str(insurance)
plot(insurance)

insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker, levels = c("no","yes"), labels = c(0,1))
insurance$region <- as.factor(insurance$region)


# based on the pair plot, age and charges, and bmi and charges seem to have relationship.
# however, in the real world, one's smoking status may affect health hence could affect the judgement of insurance providers towards their future health and make their insurance premiums higher
# Next, one's number of children could also affect health 
# based on these 4 variables, multiple linear regression models will be trained with varying combinations of the variables

# First we set the training data and the testing data
# the training data is the data used to train the model
# the testing data is the data used to check whether the trained model is valid or not to explain the population.
# if the values produced in the testing data is around the same value as the training data's model, then the trained model is good
# Secondly, we train the models using the training data
# then adjusted r-squared value will be compared and the one with the highest and has few independent variables will be chosen
# Next, the values from the testing data will be placed in the model for prediction and comparison
# Finally, if the model passes the comparing test, the model will succeed
# If otherwise, the variables used in the model will be transformed and the model goes through training again

set.seed(111)

indices <- sample(1:nrow(insurance), 0.75 * nrow(insurance))

insurance_train <- insurance[indices, ]
insurance_test <- insurance[-indices, ]
str(insurance_train)
str(insurance_test)


insurance_lm1 <- lm(charges ~ age + bmi, data = insurance_train)
summary(insurance_lm1)
# 2.2e-16
# 0.1298

insurance_lm2 <- lm(charges ~ age + bmi + children, data = insurance_train)
summary(insurance_lm2)
# < 2.2e-16
# 0.133

insurance_lm3 <- lm(charges ~ age + bmi + smoker, data = insurance_train)
summary(insurance_lm3)
# < 2.2e-16
# 0.7522

insurance_lm4 <- lm(charges ~ age + children, data = insurance_train)
summary(insurance_lm4)
# < 2.2e-16
# 0.1042

insurance_lm5 <- lm(charges ~ age + children + smoker, data = insurance_train)
summary(insurance_lm5)
# < 2.2e-16
# 0.7268

insurance_lm6 <- lm(charges ~ age + smoker, data = insurance_train)
summary(insurance_lm6)
# < 2.2e-16
# 0.7246

insurance_lm7 <- lm(charges ~ children + smoker, data = insurance_train)
summary(insurance_lm7)
# < 2.2e-16
# 0.6223

insurance_lm8 <- lm(charges ~ bmi + children + smoker, data = insurance_train)
summary(insurance_lm8)
# < 2.2e-16
# 0.6661

insurance_lm9 <- lm(charges ~ bmi + children, data = insurance_train)
summary(insurance_lm9)
# 1.172e-13
# 0.04905

insurance_lm10 <- lm(charges ~ bmi + smoker, data = insurance_train)
summary(insurance_lm10)
# < 2.2e-16
# 0.6624

insurance_lm <- lm(charges ~ age + bmi + children + smoker, data = insurance_train)
summary(insurance_lm)
# < 2.2e-16
# 0.7547


#based on linear regression principles,
#the adjusted R-squared value is used to compare the performance of the models
#when the number of independent variables for the compared models is not the same

#the adjusted R-squared value is the level that the model is confident of explaining the population.
#this means that the higher the adjusted R-squared value, the better the model can predict the population.
#based on the linear regression models trained,
#the one with the highest R-squared is insurance_lm and second highest is insurance_lm3
#insurance_lm3 is chosen as the model as it has fewer variables compared to insurance_lm but still has an adjusted R-squared value that is similar to insurance_lm

plot(insurance_lm3)

## checking normality of residuals:
hist(insurance_lm3$residuals, xlab = "Residuals", main = "Residuals Distribution")
qqnorm(insurance_lm3$residuals, main = "Q-Q Plot of Residuals")
qqline(insurance_lm3$residuals)

# based on the histogram, the residuals are normal
# however, the qqplot shows that quite some residuals are far from the line but the majority seems to be close to the line

## transforming response variable:
insurance2 <- insurance

library(MASS)
boxcox(insurance_lm3)
# 0.15-0.2

insurance2$charges <- insurance2$charges^0.15

set.seed(111)

indices <- sample(1:nrow(insurance2), 0.75 * nrow(insurance2))

insurance2_train <- insurance2[indices, ]
insurance2_test <- insurance2[-indices, ]
str(insurance2_train)
str(insurance2_test)

insurance_lm3_2 <- lm(charges ~ age + bmi + smoker, data = insurance2_train)
summary(insurance_lm3_2)
# < 2.2e-16
# 0.7599

hist(insurance_lm3_2$residuals, xlab = "Residuals", main = "Residuals Distribution")
qqnorm(insurance_lm3_2$residuals, main = "Q-Q Plot of Residuals")
qqline(insurance_lm3_2$residuals)

## checking equal variance of residuals:
plot(insurance_lm3_2$fitted.values, insurance_lm3_2$residuals, ylab = "Residuals", xlab = "Fitted Values", main = "Residuals Distribution")
abline(0, 0, lwd = 3)

# it does seem like there is non equal variance in the residuals distribution
# because the plot creates a funnel shape.
# non equal variance means there is heteroscedasticity in the data which in simpler terms means that the model did not capture any underlying patterns in the data
# heteroscedasticity could lead to unreliable standard errors which means that the standard deviation predicted by the model may be wrong
# however, the main goal is to use linear regression to predict the insurance cost based on one's health information (age, bmi, smoking status)
# it is still possible to use the model for predictions but with caution.
# it is advised to inspect whether the assumptions made by the model are reasonable.


insurance_lm3_2
confint(insurance_lm3_2)

predictions_predict <- predict.lm(insurance_lm3_2, newdata = insurance2_test, interval = "predict")
predictions_predict[1:10, ]
insurance2_test[1:10, ]

# Evaluate the model performance on the testing data
mse_test <- mean((predictions_predict - insurance2_test$charges)^2)
rsquared_test <- cor(predictions_predict, insurance2_test$charges)^2

cat("Mean Squared Error (Testing):", mse_test, "\n")
cat("R-squared (Testing):", rsquared_test, "\n")

