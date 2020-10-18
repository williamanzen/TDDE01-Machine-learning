RNGversion('3.5.1')

#url <- "https://cran.r-project.org/src/contrib/Archive/tree/tree_1.0-39.tar.gz"
#pkgFile <- "tree_1.0-39.tar.gz"
#download.file(url = url, destfile =pkgFile)
# #install.packages(pkgs=pkgFile, type="source", repos=NULL)
# #install.packages("tree")
#  library(tree)
# install.packages("e1071")
#  library(e1071)
#  library(MASS)

## Setting up environment, split data into train and test and validation
## Testing the difference between tree models.

creditscore=read.csv2("creditscoring.csv")
n=dim(creditscore)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=creditscore[id,]

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1,floor(n*0.25))
valid=creditscore[id2,]

id3=setdiff(id1,id2)
test=creditscore[id3,]

MisCalcRate <- function(confusion_table, inputdata){
  n=length(inputdata[,1])
  return(1-sum(diag(confusion_table))/n)
}

fit_tree_gini= tree(good_bad~., data=train , split="gini")
plot(fit_tree_gini)
text(fit_tree_gini,pretty=0)
#fit_tree_gini
summary(fit_tree_gini)

fit_tree_devi= tree(good_bad~., data=train , split="deviance")
plot(fit_tree_devi)
text(fit_tree_devi,pretty=0)
fit_tree_devi
plot(fit_tree_devi)
text(fit_tree_devi, pretty=0)
summary(fit_tree_devi)


Yfit_gini_train <-predict(fit_tree_gini, train,type="class")
conf_gini_train <- table(train$good_bad,Yfit_gini_train)
conf_gini_train
MisCalcRate(conf_gini_train,train)


Yfit_gini_test <-predict(fit_tree_gini, test,type="class") 
conf_gini_test <- table(test$good_bad,Yfit_gini_test)
conf_gini_test
MisCalcRate(conf_gini_test,test)


yFitDeviTrain <-predict(fit_tree_devi, train,type="class")
confDeviTrain <- table(train$good_bad,yFitDeviTrain)
confDeviTrain
MisCalcRate(confDeviTrain,train)


yFitDeviTest <-predict(fit_tree_devi, test,type="class")
confDeviTest <- table(test$good_bad,yFitDeviTest)
confDeviTest
MisCalcRate(confDeviTest,test)

trainScore= rep(0,15)
testScore= rep(0,15)

set.seed(12345)
## Prune the tree where we have the optimal amount of leaves
for(i in 2:15) {
  prunedTree=prune.tree(fit_tree_devi, best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

plot(2:15, trainScore[2:15], type="b", col="red", ylim=c(200,650))
points(2:15, testScore[2:15], type="b", col="blue")


#optimal tree 4 leaves for test data
optimalTree <- prune.tree(fit_tree_devi, best=4)
yFitOpti <- predict(optimalTree, newdata=test, type="class")
confOpti <- table(test$good_bad, yFitOpti)
confOpti
MisCalcRate(confOpti,test)
plot(optimalTree)
text(optimalTree, pretty=0)

## Predict with the optimal tree
yFitOptiP <- predict(optimalTree, newdata=test, type="vector")

## Compare it with a naive bayes classifier
naiveFit <- naiveBayes(good_bad~. , data=train)
#print(naiveFit)
naivePredTrain <- predict(naiveFit, newdata=train)
naivePredTest <- predict(naiveFit, newdata=test)
naiveConfTableTrain <- table(train$good_bad, naivePredTrain)
naiveConfTableTest <- table(test$good_bad, naivePredTest)
naiveConfTableTrain
MisCalcRate(naiveConfTableTrain, train)
naiveConfTableTest


MisCalcRate(naiveConfTableTest, test)
set.seed(12345)
yFitOptiP <- predict(optimalTree, newdata=test, type="vector")
naiveFitRaw <- predict(naiveFit, newdata = test, type="raw")


fprTree = c()
tprTree = c()
fprNaive = c()
tprNaive = c()
#compare prediction in optimal tree to seq and then compare to realvalues for FPS and TPR
for(i in seq(0.05,0.95,0.05)){
  curVal = c()
  for(p in 1:250){
  if(yFitOptiP[p,2]>i){ #ska välja good och ska välja 1-250 värden
    curVal =c(curVal, 1)
    }else {
    curVal = c(curVal, 0)
    }
  }
  # spara v?rde och g?r funktioner f?r FPR och TPR spara i numerisk vektor 1-20
  tempTable <- table(valid$good_bad, curVal)
  if(dim(tempTable)[2] == 1){
    if(colnames(tempTable) == "0"){
      tempTable <- cbind(tempTable, c(0,0))
    } else {
      tempTable <- cbind(c(0,0), tempTable)
    }
  }
  if(dim(tempTable)[2] == 2){ 
    print(tempTable)
    tprTree= c(tprTree, tempTable[2,2]/(tempTable[2,2]+tempTable[2,1]))
    fprTree= c(fprTree, tempTable[1,2]/(tempTable[1,2]+tempTable[1,1]))
  }
}

#compare prediction in naive to seq and then compare to realvalues for FPS and TPR
for(i in seq(0.05,0.95,0.05)){
  curVal = c()
  for(p in 1:250){
    if(naiveFitRaw[p,2]>i){ #ska välja good och ska välja 1-250 värden
      curVal =c(curVal, 1)
    }else {
      curVal = c(curVal, 0)
    }
  }
  
  tempTable <- table(test$good_bad, curVal) # spara v?rde och g?r funktioner f?r FPR och TPR spara i numerisk vektor 1-20
  
  if(dim(tempTable)[2] == 2){ 
    
    tprNaive = c(tprNaive, tempTable[2,2]/rowSums(tempTable)[2])
    fprNaive = c(fprNaive, tempTable[1,2]/rowSums(tempTable)[1])
  }
  
}

fprTree<-sort(fprTree)
tprTree<-sort(tprTree)
# fprNaive<-sort(fprNaive)
# tprNaive<-sort(tprNaive)

plot(fprTree,tprTree, xlab="FPR", ylab="TPR", col="blue", xlim=c(0,1.1), ylim=c(0,1.1), type="l")
points(fprNaive,tprNaive,col="red",type="l")
legend("topleft",legend=c("Tree", "Naive"), col=c("blue", "red"),lty=c(1,1))

## LossMatrix implementation
lossMatrix <- matrix(c(0, 1,10, 0), 2, 2)
naiveFit <- naiveBayes(good_bad~. , data = train)
naiveLossTrain <- predict(naiveFit, newdata = train,type="raw")
naiveLossTest <- predict(naiveFit, newdata = test,type="raw")

naiveLossTrainConf <- table(train$good_bad, naiveLossTrain[,2]/naiveLossTrain[,1] > 10)
naiveLossTestConf <- table(test$good_bad, naiveLossTest[,2]/naiveLossTest[,1] > 10)

naiveLossTrainConf
MisCalcRate(naiveLossTrainConf,train) #0.546


naiveLossTestConf
MisCalcRate(naiveLossTestConf,test) #0.508
