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

# #### Creating binary good/bad variable ####
#this will be used later on for classification :)
wines$good <- ifelse(wines$quality >= 7, 1, 0)
hist(wines$good)



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

#lets try a reduced model with these features
fit.reduced <- lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol, wines)
summary(fit.reduced)

#residuals (still bad), only slightly worse than the full model


#lets try using classification instead...
#the kaggle post suggested creating a binary feature for good (1) and not good
#to do so, we need to set a cutoff value for this
#lets say that a quality <=7 is good, and is bad otherwise...

#TODO
#create new binary good/bad feature
#classification models
#KNN, Random forest, logistic regression....

