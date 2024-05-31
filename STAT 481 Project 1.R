# Andrew Gascon
# STAT 481 Project 1
# March 29, 2024

# Question: Using backward selection, can we derive a simplified multilinear
# model that still does a good job at determining a cereal brand's nutrition
# rating compared to the original full model?

# Set the working directory and load the data.
setwd("C:/Users/Owner/Downloads/STAT 481")
cereal <- read.csv(file = "CerealsRating.csv", header = TRUE)

# y: rating
# x: protein (x1), sodium (x2), fiber (x3), sugars (x4), vitamins (x5),
#    weight (x6), cups (x7)

# Select only the columns we're interested in:
analysis <- subset(cereal, select = c("rating", "protein", "sodium", "fiber",
                                      "sugars", "vitamins", "weight", "cups"))

# Convert columns into variables
y <- analysis$rating    # y = rating
x1 <- analysis$protein  # x1 = protein
x2 <- analysis$sodium   # x2 = sodium
x3 <- analysis$fiber    # x3 = fiber

x4 <- analysis$sugars   # x4 = sugars
x5 <- analysis$vitamins # x5 = vitamins
x6 <- analysis$weight   # x6 = weight
x7 <- analysis$cups     # x7 = cups

# Number of NA values in y (rating):
length(which(y == -1))

# Number of NA values in x1 (protein): 0
length(which(x1 == -1))

# Number of NA values in x2 (sodium): 0
length(which(x2 == -1))

# Number of NA values in x3 (fiber): 0
length(which(x3 == -1))

# Number of NA values in x4 (sugars): 1
length(which(x4 == -1))

# Number of NA values in x5 (vitamins): 0
length(which(x5 == -1))

# Number of NA values in x6 (weight): 0
length(which(x6 == -1))

# Number of NA values in x7 (cups): 0
length(which(x7 == -1))

# There's a missing value in sugars (x4), so we'll replace it with a 0.
analysis$sugars[x4 == -1] <- 0

# Descriptive statistics:

# Rating (y)
fivenum(y)
mean(y)
sd(y)

par(mfrow = c(2, 1))
hist(y, xlab = "Rating (y)", ylab = "Frequency",
     main = "Distribution of Rating (y)")

ybp <- boxplot(y, ylab = "Rating (y)", main = "Distribution of Rating (y)")
length(ybp$out) # Number of outliers in Rating (y): 1

# Protein (x1)
fivenum(x1)
mean(x1)
sd(x1)

hist(x1, xlab = "Protein (x1)", ylab = "Frequency",
     main = "Distribution of Protein (x1)")

x1bp <- boxplot(x1, ylab = "Protein (x1)", main = "Distribution of Protein (x1)")
length(x1bp$out) # Number of outliers in Protein (x1): 3

# Sodium (x2)
fivenum(x2)
mean(x2)
sd(x2)

hist(x2, xlab = "Sodium (x2)", ylab = "Frequency",
     main = "Distribution of Sodium (x2)")

x2bp <- boxplot(x2, ylab = "Sodium (x2)", main = "Distribution of Sodium (x2)")
length(x2bp$out) # Number of outliers in Sodium (x2): 9

# Fiber (x3)
fivenum(x3)
mean(x3)
sd(x3)

hist(x3, xlab = "Fiber (x3)", ylab = "Frequency",
     main = "Distribution of Fiber (x3)")

x3bp <- boxplot(x3, ylab = "Fiber (x3)", main = "Distribution of Fiber (x3)")
length(x3bp$out) # Number of outliers in Fiber (x3): 3

# Sugars (x4)
fivenum(x4)
mean(x4)
sd(x4)

hist(x4, xlab = "Sugars (x4)", ylab = "Frequency",
     main = "Distribution of Sugars (x4)")

x4bp <- boxplot(x4, ylab = "Sugars (x4)", main = "Distribution of Sugars (x4)")
length(x4bp$out) # Number of outliers in Sugars (x4): 0

# Vitamins (x5)
fivenum(x5)
mean(x5)
sd(x5)

hist(x5, xlab = "Vitamins (x5)", ylab = "Frequency",
     main = "Distribution of Vitamins (x5)")

x5bp <- boxplot(x5, ylab = "Vitamins (x5)", main = "Distribution of Vitamins (x5)")
length(x5bp$out) # Number of outliers in Vitamins (x5): 14

# Weight (x6)
fivenum(x6)
mean(x6)
sd(x6)

hist(x6, xlab = "Weight (x6)", ylab = "Frequency",
     main = "Distribution of Weight (x6)")

x6bp <- boxplot(x6, ylab = "Weight (x6)", main = "Distribution of Weight (x6)")
length(x6bp$out) # Number of outliers in Weight (x6): 13

# Cups (x7)
fivenum(x7)
mean(x7)
sd(x7)

hist(x7, xlab = "Cups (x7)", ylab = "Frequency",
     main = "Distribution of Cups (x7)")

x7bp <- boxplot(x7, ylab = "Cups (x7)", main = "Distribution of Cups (x7)")
length(x7bp$out) # Number of outliers in Cups (x7): 1

# Full model and model check.
model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = analysis)
par(mfrow = c(1, 1))
plot(model)

# Multicolinearity check
library(car)
vif(model)

# Variable selection:
summary(model)
anova(model)

# Backward selection:
back <- step(model, direction = "backward", trace = 0)
summary(back)
anova(back)

# Model check for the simplified model.
par(mfrow = c(1, 1))
plot(back, ask = FALSE)

# AIC and BIC comparison between the two models
AIC(model) # 461.5946
BIC(model) # 482.6889

AIC(back) # 457.3878
BIC(back) # 471.4506