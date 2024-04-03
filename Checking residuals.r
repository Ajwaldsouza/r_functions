# Checking residuals
# Creating a function that extracts the residuals from a specified model, and plots the necessary residual plots for assumption checking of the models. 

# The assumptions checked here include normality of residuals (via Q-Q plot and Shapiro-Wilk test), homogeneity of variance (plotting treatment variances), and independence of observations (plotting residuals vs predicted values)

residual_check <- function(x) {
residual <-  resid(x)
predicted <- predict(x)


par(mfrow = c(2,2))

plot(predicted, residual)
abline(0,0)

boxplot(residual~trmt, data =yield)


qqnorm(residual)
qqline(residual, col="steelblue")
shapiro.test(residual)
}

residual_check(model)


# This function can be used for both fixed effects and mixed effects models derived as 'aov' or 'lm' objects. 