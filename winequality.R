setwd("C:/Users/Jordan/Desktop/Sideprojects/")

# ####Reading Data#####
wines <- read.csv("winequality-red.csv")
summary(wines)

# ####EXPLORATORY PLOTS ####
hist(wines$quality)
plot(quality ~ alcohol, wines)
plot(quality ~ residual.sugar, wines)

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
