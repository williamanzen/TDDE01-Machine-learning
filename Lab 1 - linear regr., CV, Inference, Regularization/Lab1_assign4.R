n=dim(tecator)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=tecator[id,]
test=tecator[-id,]

plot(x = tecator$Moisture, y = tecator$Protein)

## Testing different polynomial degrees for fitting the data
## And finally selecting the model with least MSE
fit_1 = lm(Moisture~poly(Protein,degree=1), data=train)
fit_2 = lm(Moisture~poly(Protein,degree=2), data=train)
fit_3 = lm(Moisture~poly(Protein,degree=3), data=train)
fit_4 = lm(Moisture~poly(Protein,degree=4), data=train)
fit_5 = lm(Moisture~poly(Protein,degree=5), data=train)
fit_6 = lm(Moisture~poly(Protein,degree=6), data=train)

predict_train_1 <- predict(fit_1, train)
predict_test_1 <- predict(fit_1, test)

predict_train_2 <- predict(fit_2, train)
predict_test_2 <- predict(fit_2, test)

predict_train_3 <- predict(fit_3, train)
predict_test_3 <- predict(fit_3, test)

predict_train_4<- predict(fit_4, train)
predict_test_4 <- predict(fit_4, test)

predict_train_5 <- predict(fit_5, train)
predict_test_5 <- predict(fit_5, test)

predict_train_6 <- predict(fit_6, train)
predict_test_6 <- predict(fit_6, test)

mse1_train<-mean((train$Moisture - predict_train_1)^2)
mse1_test<-mean((test$Moisture - predict_test_1)^2)

mse2_train<-mean((train$Moisture - predict_train_2)^2)
mse2_test<-mean((test$Moisture - predict_test_2)^2)

mse3_train<-mean((train$Moisture - predict_train_3)^2)
mse3_test<-mean((test$Moisture - predict_test_3)^2)

mse4_train<-mean((train$Moisture - predict_train_4)^2)
mse4_test<-mean((test$Moisture - predict_test_4)^2)

mse5_train<-mean((train$Moisture - predict_train_5)^2)
mse5_test<-mean((test$Moisture - predict_test_5)^2)

mse6_train<-mean((train$Moisture - predict_train_6)^2)
mse6_test<-mean((test$Moisture - predict_test_6)^2)

mse_train <- c(mse1_train, mse2_train, mse3_train, mse4_train, mse5_train, mse6_train)
mse_test <- c(mse1_test, mse2_test, mse3_test, mse4_test, mse5_test, mse6_test)

plot(mse_train, type="o", col="red", ylim=c(15,50) , ylab="MSE" )
lines(mse_test, type="o", col="blue")

legend(1,50,c("train","test"), col=c("red","blue"), pch=21:22, lty=1:1)

# install.packages("MASS")
 library(MASS)

subset_tecator <- subset(tecator, select = -c(Moisture, Protein))
fit_Fat <- lm(Fat~.,data=subset_tecator)

## Reducing number of variables in the dataset with stepAIC.
step <- stepAIC(fit_Fat , direction="both")
step$anova
summary(step)

# install.packages("glmnet")
library(glmnet)

covariates=scale(subset_tecator[,2:101])
response=scale(subset_tecator[,102])

## Testing the different between Ridge and Lasso in how it penalizes variables
## and finally choosing the optimal lambda by cross validation.

ridge_model=glmnet(as.matrix(covariates),response,alpha=0,family="gaussian")
plot(ridge_model,xvar="lambda",label=TRUE)

lasso_model=glmnet(as.matrix(covariates),response,alpha=1,family="gaussian")
plot(lasso_model, xvar="lambda",label=TRUE)

lasso_cv_model = cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian", lambda=seq(0,1,0.001))
lasso_cv_model$lambda.min
plot(lasso_cv_model)
coef(lasso_cv_model, s="lambda.min")


