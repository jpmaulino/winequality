setwd("C:/Users/Jordan/Desktop/Sideprojects/winequality")

library(corrplot)

# ####Reading Data#####
wines <- read.csv("winequality-red.csv")
summary(wines)

# ####EXPLORATORY ####
hist(wines$quality)
plot(wines)
plot(quality ~ alcohol, wines)
plot(quality ~ residual.sugar, wines)

corrplot(cor(wines))

#alcohol content seems to have a positive correlation to quality
#volatile acidity appears to have a negative correlation

# ####SPLITTING DATA ####
set.seed(1)

size <- floor(nrow(wines) * .75)
samples <- sample(nrow(wines), size)
train <- wines[samples,]
test <- wines[-samples,]

# ####LINEAR REGRESSION ####

#full model
fit.full <- lm(quality ~., wines)
summary(fit.full)
anova(fit.full)

#volatile acidity, chlorides, total sulfur dioxide, sulphates, and alcohol are most significant
