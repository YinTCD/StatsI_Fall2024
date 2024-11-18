# Install and load the car package
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

# Create the professional variable
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Fit the linear model
model <- lm(prestige ~ income * professional, data = Prestige)
summary(model)

# Extract coefficients
coef <- coef(model)
beta_0 <- coef["(Intercept)"]
beta_1 <- coef["income"]
beta_2 <- coef["professional"]
beta_3 <- coef["income:professional"]

# Part (f): Effect of $1,000 increase in income for professionals
income_increase <- 1000
effect_income_prof <- income_increase * (beta_1 + beta_3)

# Part (g): Effect of switching to professional at income = $6,000
income_value <- 6000
effect_prof <- beta_2 + income_value * beta_3

# Output results
cat("Effect of $1,000 increase in income for professionals:", effect_income_prof, "\n")
cat("Effect of switching to professional at $6,000 income:", effect_prof, "\n")
