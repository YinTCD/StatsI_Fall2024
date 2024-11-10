#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#Question 1
# Run the regression
model <- lm(voteshare ~ difflog, data = inc.sub)

# Display the summary of the regression results
summary(model)

install.packages("ggplot2")

# Load ggplot2 for plotting
library(ggplot2)

pdf("Scatterplot with regression line.pdf")
# Create scatterplot with regression line
ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(color = "blue", alpha = 0.6) +          # Scatter plot with semi-transparent blue points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regression line in red with confidence interval
  labs(title = "Scatterplot of Vote Share vs. Difflog",
       x = "Difflog",
       y = "Incumbent's Vote Share") +
  theme_minimal()         # A clean, minimal theme for the plot
dev.off()

# Save the residuals in a separate object
residuals_model <- residuals(model)

# Display the first few residuals to confirm
head(residuals_model)


#Question 2
# Run the regression
model2 <- lm(presvote ~ difflog, data = inc.sub)

# Display the summary of the regression results
summary(model2)

pdf("Scatterplot with regression line2.pdf")
# Create scatterplot with regression line
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(color = "blue", alpha = 0.6) +          # Scatter plot with semi-transparent blue points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regression line in red with confidence interval
  labs(title = "Scatterplot of Presvote vs. Difflog",
       x = "Difflog",
       y = "Presvote") +
  theme_minimal()         # A clean, minimal theme for the plot
dev.off()

# Save the residuals in a separate object
residuals_model2 <- residuals(model2)

# Display the first few residuals to confirm
head(residuals_model2)

#Question 3
# Run the regression
model3 <- lm(voteshare ~ presvote, data = inc.sub)

# Display the summary of the regression results
summary(model3)

pdf("Scatterplot with regression line3.pdf")
# Create scatterplot with regression line
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(color = "blue", alpha = 0.6) +          # Scatter plot with semi-transparent blue points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regression line in red with confidence interval
  labs(title = "Scatterplot of Voteshare vs. Presvote",
       x = "Presvote",
       y = "Voteshare") +
  theme_minimal()         # A clean, minimal theme for the plot
dev.off()

#Question 4
# Run the regression
model4 <- lm(residuals_model ~ residuals_model2, data = inc.sub)

# Display the summary of the regression results
summary(model4)

pdf("Scatterplot with regression line4.pdf")
# Create scatterplot with regression line
ggplot(inc.sub, aes(x = residuals_model2, y = residuals_model)) +
  geom_point(color = "blue", alpha = 0.6) +          # Scatter plot with semi-transparent blue points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regression line in red with confidence interval
  labs(title = "Scatterplot of Residuals_model vs. Residuals_model2",
       x = "Residuals_model2",
       y = "Residuals_model") +
  theme_minimal()         # A clean, minimal theme for the plot
dev.off()

#Question 5
# Run the regression
model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# Display the summary of the regression results
summary(model5)
