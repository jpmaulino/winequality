setwd("C:/Users/Jordan/Desktop/Sideprojects/winequality")

library(corrplot)
library(pROC)
library(lattice)
library(ggplot2)

library(caret)

# ####Reading Data#####
wines <- read.csv("winequality-red.csv")
summary(wines)

# #### Creating binary good/bad variable ####
#this will be used later on for classification :)
wines$good <- ifelse(wines$quality >= 7, 1, 0)
hist(wines$good)

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

#lets try a reduced model with these features
fit.reduced <- lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol, wines)
summary(fit.reduced)

#residuals (still bad), only slightly worse than the full model
#but this tells us that most of the variation in the data is indeed caused by these variables



#lets try using classification instead...
#the kaggle post suggested creating a binary feature for good (1) and not good
#to do so, we need to set a cutoff value for this
#lets say that a quality <=7 is good, and is bad otherwise...




# #### LOGISTIC REGRESSION ####

#lets try fitting a logistic regression model to the data using our significant 
#variables from earlier :)

#volatile acidity, chlorides, total sulfur dioxide, sulphates, and alcohol are most significant

log.fit <- glm(good ~ volatile.acidity + chlorides + total.sulfur.dioxide + sulphates +alcohol, train, family=binomial(link="logit"))
summary(log.fit)

slog.fit <- pnorm(predict(log.fit))

roc <- plot.roc(train$good, slog.fit, main="AUC curve", percent=TRUE,ci=TRUE,print.auc=TRUE)
roc.se <- ci.se(roc, specificities = seq(0,100,5))
plot(roc.se, type="shape", col="blue")

#AUC is 87%! preeeety good 

glm.probs <- predict(log.fit, newdata = test, type="response")

#create vector of class predictions
glm.pred <- rep(0, nrow(test))
glm.pred[glm.probs > .5] <- 1

confusionMatrix(glm.pred, test$good)


#we see our logisitic regresion model yields an 86% accuracy!


# #### K-nearest Neighbors ####
library(class)
knn10 <- knn(train, test, cl = train$good, k=10)
table(test$good, knn10)
mean(test$good == knn10)

knn20 <- knn(train, test, cl=train$good, k=20)
table(test$good, knn20)
mean(test$good == knn20)

#knn with k=10 yields 89% accuracy


#TODO
#random forest..

