#install.packages("kknn")
#library("kknn")
Dataframe=read.csv2("spambasecsv.csv")
n=dim(Dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]

## Training a Linear model to fit the data
formula <- Spam~.
fit <- glm(formula, data = train, family = 'binomial')
summary(fit)

## Predicting outcomes with the model
predict_test <-predict(fit, test, type = 'response')
predict_train <- predict(fit, train, type = 'response')

## Function for calculating the miscalculation rate, Percentage of misses
MisCalcRate <- function(confusion_table, inputdata){
  n=length(inputdata[,1])
  return(1-sum(diag(confusion_table))/n)
}

first_trainconfusion <- table(train$Spam, predict_train > 0.5)
first_trainconfusion
first_train_miscalcrate <- MisCalcRate(first_trainconfusion,train)
print(first_train_miscalcrate) # 0.1605839

first_testconfusion <- table(test$Spam, predict_test > 0.5)
first_testconfusion
first_test_miscalcrate <- MisCalcRate(first_testconfusion,test)
print(first_test_miscalcrate) # = 0.1715328

second_trainconfusion <- table(train$Spam, predict_train > 0.8)
second_trainconfusion
second_train_miscalcrate <- MisCalcRate(second_trainconfusion,train)
print(second_train_miscalcrate) # = 0.250365

second_testconfusion <- table(test$Spam, predict_test > 0.8)
second_testconfusion
second_test_miscalcrate <- MisCalcRate(second_testconfusion,train)
print(second_test_miscalcrate) # = 0.2759124

library(kknn)
neighbours_train <- kknn(Spam~., train, train, k=30 )
neighbours_test <- kknn(Spam~., train, test, k=30 )

first_train_kknconfusion = table(train$Spam, neighbours_train[["fitted.values"]]>0.5)
first_train_kknconfusion
firsttrain_kkn_miscalcrate <- MisCalcRate(first_train_kknconfusion, train)
print(firsttrain_kkn_miscalcrate) # 0.1671533

first_test_kknconfusion = table(test$Spam, neighbours_test[["fitted.values"]]>0.5)
first_test_kknconfusion
firsttest_kkn_miscalcrate <- MisCalcRate(first_test_kknconfusion, test)
print(firsttest_kkn_miscalcrate) # =0.3131387

neighbours_two_train <- kknn(Spam~., train, train, k=1 )
neighbours_two_test <- kknn(Spam~., train, test, k=1 )

second_train_kknconfusion = table(train$Spam, neighbours_two_train[["fitted.values"]]>0.5)
second_train_kknconfusion
secondtrain_kkn_miscalcrate <- MisCalcRate(second_train_kknconfusion, train)
print(secondtrain_kkn_miscalcrate) # = 0

second_test_kknconfusion = table(test$Spam, neighbours_two_test[["fitted.values"]]>0.5)
second_test_kknconfusion
secondtest_kkn_miscalcrate <- MisCalcRate(second_test_kknconfusion,test)
print(secondtest_kkn_miscalcrate) # = 0.3591241



