# 1511 Final Project Script
install.packages("table1")
install.packages("car")
install.packages("qpcR")

library(car)
library(broom)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(table1)

# ONGOING QUESTIONS
# Small r-squared value with some significant results?
# https://www.theanalysisfactor.com/small-r-squared/
# https://www.theanalysisfactor.com/insignificant-effects-in-model/
# https://web.ma.utexas.edu/users/mks/statmistakes/regressioncoeffs.html
# Log transform some but not all variables -- log transform population?
# Keep population in (despite failed multicollinearity test) model as control?

# RESPONSE VAR
# beta regression for percentage of confirmed cases? http://rcompanion.org/handbook/J_02.html
# log ratio transform data? https://www.researchgate.net/post/Statistical_test_on_percentage_data_what_to_use
# use GLM with special logistic link and binomail dist (special logistic regression) https://www.theanalysisfactor.com/when-to-use-logistic-regression-for-percentages-and-counts/
# USE THIS: use raw counts from confirmed percentages and treat population as covariate in model -> multivariate lm

# ASSUMPTIONS
# Variables not normally distributed -> should be okay: https://data.library.virginia.edu/normality-assumption/
# If we want normally distributed: consider transform: log(confirmed + some constant)
# Could also consider performing quantile regression -> w/ median

#-BEGIN SCRIPT-----------------------------------------------------------------#

# Load in and initially inspect the data
setwd("~/OneDrive/documents/browncs/PHP1511/")
energyData <- read.csv("~/OneDrive/documents/browncs/PHP1511/Food_Supply_kcal_Data.csv")
head(energyData)

#-DATA CLEANING----------------------------------------------------------------#

# Drop unneeded columns
energyData <- subset(energyData, select = c(Country, Fruits...Excluding.Wine, Vegetables, Obesity, Undernourished, Confirmed, Population))

# Check if there are any null values in the dataset
colSums(is.na(energyData)) 
which(is.na(energyData))

# Handle missing data and check null again
energyData <- energyData[complete.cases(energyData),]
which(is.na(energyData))

# Check data format and cast 'Undernourished' column to num (specifically, < 2.5 -> 2.5)
str(energyData)
energyData <- energyData %>% mutate(Undernourished = as.numeric(gsub("<", "", Undernourished)))
as.numeric(energyData$Undernourished)
str(energyData)

# Join (% energy intake) Fruits and Vegetable columns
energyData$FruitsAndVegetables <- energyData$Fruits...Excluding.Wine + energyData$Vegetables
energyData <- subset(energyData, select = -c(Fruits...Excluding.Wine, Vegetables))
str(energyData)

#-EXPLORING DATA---------------------------------------------------------------#

# Explore distributions of population size, undernourished count, obesity count, confirmed count
ggplot(energyData, aes(Obesity)) + geom_histogram(bins=30) + ggtitle("Obesity Rate Distribution")
ggplot(energyData, aes(Undernourished)) + geom_histogram(bins=30) + ggtitle("Undernourished Rate Distribution")
ggplot(energyData, aes(Population)) + geom_histogram(bins=30) + ggtitle("Population Size Distribution")
ggplot(energyData, aes(Confirmed)) + geom_histogram(bins=30) + ggtitle("Confirmed Percentage Distribution")
ggplot(energyData, aes(FruitsAndVegetables)) + geom_histogram(bins=30) + ggtitle("Percent Energy Intake of Fruits and Vegetables Distribution")
# NOTE: every distribution but percent energy intake is skewed

# Generate table1
colnames(energyData)
table1(~FruitsAndVegetables + Obesity + Undernourished + Confirmed + Population, data=energyData)

# Determine country with highest/lowest undernourished rates and obesity rates and COVID-19 confirmed rates
highUndernourished <- energyData[which.max(energyData$Undernourished),]
print(highUndernourished)

# NOTE: multiple countries have an undernourished rate = 2.5
# lowUndernourished <- energyData[which.min(energyData$Undernourished),]
# print(lowUndernourished)

highObesity <- energyData[which.max(energyData$Obesity),]
print(highObesity)
lowObesity <- energyData[which.min(energyData$Obesity),]
print(lowObesity)

highConfirmed <- energyData[which.max(energyData$Confirmed),]
print(highConfirmed)
lowConfirmed <- energyData[which.min(energyData$Confirmed),]
print(lowConfirmed)

#-RUN STATISTICAL TESTS--------------------------------------------------------#
# Use Wilcoxon rank-sum test to test below-average obesity countries and above-average
# obesity countries' undernourished rates

# Divide data into 2 groups based on global obesity rate (13%)
below_obesity_countries <- energyData %>% filter(Obesity < 13)
above_obesity_countries <- energyData %>% filter(Obesity >= 13)

# Run test -> reject null
wilcox.test(below_obesity_countries$Undernourished, above_obesity_countries$Undernourished, 
            alternative="two.sided", paired=FALSE, conf.level=0.95, conf.int=TRUE)

# Use one-way ANOVA to test below-average undernourished countries and above-average
# undernourished countries' percent energy intake of fruits and vegetables

# Divide data into 2 groups based on median of undernourished rate
med_undernourished <- median(energyData$Undernourished)

below_undernourished_countries <- energyData %>% filter(Undernourished < med_undernourished)
above_undernourished_countries <- energyData %>% filter(Undernourished >= med_undernourished)

# Run Levene's test for equality of variances
combined_undernourished_countries <- c(below_undernourished_countries$FruitsAndVegetables, above_undernourished_countries$FruitsAndVegetables)
group <- as.factor(c(rep(1, length(below_undernourished_countries$FruitsAndVegetables)), rep(2, length(above_undernourished_countries$FruitsAndVegetables))))
leveneTest(combined_undernourished_countries, group)

# Run one-way ANOVA
grouped_data <- bind_rows(below_undernourished_countries, above_undernourished_countries)
grouped_data$level <- group
grouped_data$level <- as.factor(grouped_data$level)

anova1 <- aov(FruitsAndVegetables ~ level, grouped_data)
summary(anova1)

#-RUN REGRESSION MODEL---------------------------------------------------------#
# Cast confirmed to counts (instead of percentages)
energyData <- transform(energyData, Confirmed = (Confirmed / 100) * Population)
energyData <- transform(energyData, Undernourished = (Undernourished / 100) * Population)
energyData <- transform(energyData, Obesity = (Obesity / 100) * Population)
str(energyData)

# Transform Obesity, Undernourished, Confirmed, and Population variables
energyData$Obesity <- log(energyData$Obesity)
energyData$Undernourished <- log(energyData$Undernourished)
energyData$Confirmed <- log(energyData$Confirmed)
energyData$Population <- log(energyData$Population)

# Re-plot distributions
ggplot(energyData, aes(Obesity)) + geom_histogram(bins=30) + ggtitle("Log Obesity Distribution")
ggplot(energyData, aes(Undernourished)) + geom_histogram(bins=30) + ggtitle("Log Undernourished Distribution")
ggplot(energyData, aes(Population)) + geom_histogram(bins=30) + ggtitle("Log Population Size Distribution")
ggplot(energyData, aes(Confirmed)) + geom_histogram(bins=30) + ggtitle("Log Confirmed Distribution")

# Build and assess model 1
model1 <- lm(Confirmed ~ Obesity + Undernourished + Population + FruitsAndVegetables, data=energyData)
tidy1 <- tidy(model1, conf.int=T)[, -c(3:4)]
tidy1

glance(model1)
sqrt(mean(model1$residuals^2))

# Check for multicollinearity with VIF values
vif(model1)

# Should be associated b/c they are a function of population size
# Remove obesity

# Build and assess model 2
model2 <- lm(Confirmed ~ Undernourished + FruitsAndVegetables + Population, data=energyData)
tidy2 <- tidy(model2, conf.int=T)[, -c(3:4)]
tidy2
sqrt(mean(model2$residuals^2))
vif(model2)

#-CHECK ASSUMPTIONS------------------------------------------------------------#
# Check linearity
plot(fitted(model2), energyData$Confirmed)

# Check homoscedasticity
plot(fitted(model2), resid(model2))

# Check independence of errors
cor(fitted(model2), resid(model2))

# Check normality of errors
qqnorm(resid(model2))

# Check mean of errors
mean(resid(model2))
