# Spotify project
setwd("~/Documents/Data-Science-Projects/Spotify")
# getwd()
rm(list=ls())
ls()
options(max.print=500)
set.seed(2000)

library(MASS) #glm(), lda(), qda() functions
library(glmnet)



# cleaning data up
spotify<-read.csv("/Users/sethketron/Documents/Data-Science-Projects/Spotify/streaming_history.csv", header=T)
spotify<-spotify[c(3,4,5,6,7,8,9,10,11,12,13,19,20)]
spotify<-spotify[!spotify$time_signature==0, ]
spotify<-spotify[!spotify$time_signature==1, ]
spotify$mode<-as.factor(spotify$mode)
spotify$key<-as.factor(spotify$key)
spotify$time_signature<-as.factor(spotify$time_signature)

summary(spotify)



# building models and plots

# validation set approach for estimating
# test error rate
index<-sample(1:nrow(spotify), 0.8*nrow(spotify))
train<-spotify[index,]
dim(train)
validation<-spotify[-index,]
dim(validation)


lin.mod<-lm(danceability ~ valence, data=spotify)

plot(spotify$valence, spotify$danceability, xlab="valence", ylab="danceability", main="Least Squares Regression Line")
abline(lin.mod, col="red")


# logistic regression with modality as the
# response and danceability as predictor.
# Test errror estimates obtained using
# validation set approach.
simplog.mod<-glm(spotify$mode ~ spotify$danceability, data=train, family=binomial)
summary(simplog.mod)

simplog.probs<-predict(simplog.mod, validation$mode, type="response")
simplog.probs

simplog.pred<-rep("0", 1451)
simplog.pred[simplog.probs>0.5]="1"

table(simplog.pred, spotify$mode)
mean(simplog.pred==spotify$mode)

plot(spotify$danceability, as.character(spotify$mode), pch=16, xlab="danceability", ylab="mode", main="Predicted Probabilities")
lines(spotify$danceability, simplog.probs, col="blue")


# fitting multiple logistic regression
# model with mode as response using
# validation set approach to estimate test
# error rate
log.mod<-glm(spotify$mode ~ spotify$key + spotify$time_signature + spotify$danceability + spotify$energy + spotify$loudness + spotify$speechiness + spotify$acousticness + spotify$instrumentalness + spotify$liveness + spotify$valence + spotify$tempo + spotify$duration_ms, data=train, family=binomial)
summary(log.mod)

log.probs<-predict(log.mod, validation$mode)
log.probs

log.pred<-rep("0", 1451)
log.pred[log.probs>0.5]="1"

table(log.pred, spotify$mode)
mean(log.pred==spotify$mode)
# log regression correctly predicts the 
# modality of the song 65% of the time on
# validation set so estimated test error
# rate is 100 - 65.3 = 34.7


# fitting lasso regression model using mode
# as response using validation set approach
# to estimate test error rate
train.pred.matrix<-model.matrix(train$mode~train$key + train$time_signature + train$danceability + train$energy + train$loudness + train$speechiness + train$acousticness + train$instrumentalness + train$liveness + train$valence + train$tempo + train$duration_ms, data=train)[,-1]
head(train.pred.matrix)

val.pred.matrix<-model.matrix(validation$mode ~ validation$key + validation$time_signature + validation$danceability + validation$energy + validation$loudness + validation$speechiness + validation$acousticness + validation$instrumentalness + validation$liveness + validation$valence + validation$tempo + validation$duration_ms, data=validation)[,-1]
head(val.pred.matrix)


# using cross validation to determine value
# of lambda that minimizes cv error
cv.loglass<-cv.glmnet(train.pred.matrix, train$mode, family="binomial", alpha=1, lambda=NULL)
plot(cv.loglass)

# fitting lasso reg using lambda.min
loglass.mod<-glmnet(train.pred.matrix, train$mode, alpha=1, family="binomial", lambda=cv.loglass$lambda.min)
coef(loglass.mod)

loglass.probs<-predict(loglass.mod, val.pred.matrix)
loglass.probs

loglass.preds<-ifelse(loglass.probs > 0.5, "1", "0")

observed.classes<-validation$mode
mean(loglass.preds==observed.classes)

cv.loglass$lambda.min
cv.loglass$lambda.1se
coef(cv.loglass, cv.loglass$lambda.min)
coef(cv.loglass, cv.loglass$lambda.1se)


# fitting lasso regression using lambda.1se
loglass.mod1<-glmnet(train.pred.matrix, train$mode, alpha=1, family="binomial", lambda=cv.loglass$lambda.1se)

loglass.probs1<-predict(loglass.mod1, val.pred.matrix)
loglass.probs1

loglass.preds1<-ifelse(loglass.probs1 > 0.5, "1", "0")

observed.classes<-validation$mode
mean(loglass.preds1==observed.classes)
