RNGversion('3.5.1')
State <- read.csv2("State.csv")
State <- State[order(State$MET),]

plot(State$EX,State$MET)

# install.packages("tree")
# library(tree)
# install.packages("cvTools")
# library(cvTools)
set.seed(12345)
treeFit <- tree(EX~MET, data=State, control=tree.control(nobs=48 , mincut=8) )
plot(treeFit)
text(treeFit)
set.seed(12345)
fitTreePruneCV <- cv.tree(treeFit, FUN=prune.tree)
plot(fitTreePruneCV$size, fitTreePruneCV$dev, type="b", xlab="Size", ylab="Deviance")
# lowest deviance with size of tree with 3 leaves

# Testing the difference between /non/-parametric bootstrap for data simulation

fitTreePruned <- prune.tree(treeFit, best=3)

hist(resid(fitTreePruned), breaks=10, main="Histogram of residuals")

regrTree <- predict(fitTreePruned, newdata=State)
plot(State$MET,State$EX , ylab="EX", xlab="MET", ylim=c(100,500), main="Black - Original, Red - Fitted")
points(State$MET, regrTree, col="red", type="b")

plot(resid(fitTreePruned), main="Residuals")
abline(h=0,lty=3)

mean(resid(treeFit))

#The residuals are a little skewed towards more residuals being negative as can be seen in the histogram. But it is
#still likely that the data is normally distributed as the bell curve can be recognized

library(boot)

## NONPARAMETRIC BOOTSTRAP

statFunction <- function(data, ind){
  data1 = data[ind,]
  fitTree <- tree( EX~MET, data=data1, control=tree.control(nobs=nrow(data), mincut=8))
  prunedTree <- prune.tree(fitTree, best=3)
  predFit = predict(prunedTree, newdata=State)
  return(predFit)
}

set.seed(12345)
bootData <- boot(data=State, statistic = statFunction, R=1000)
bootEnvelope = envelope(bootData, level = 0.95) #calc confidence interval

plot(State$MET,State$EX , ylab="EX", xlab="MET", ylim=c(100,500), main="Nonparametric bootstrap")
points(State$MET, regrTree, col="red", type="b")
lines(State$MET, bootEnvelope$point[1,], col="grey", lty=3)
lines(State$MET, bootEnvelope$point[2,], col="grey", lty=3)
legend("top", legend=c("Data", "Prediction","95% CI"), lty=c(NA,1,3),
       col=c(1,2,"grey"), cex=.7)

#The confidence interval has a lot of data outside of the interval although it is a good measurement for the mean
# of the data. The interval is bumpy around outlying data points and the width changes with the variance of the data.

## PARAMETRIC BOOTSTRAP # MLE - ranGen - statistic ?boot

## returns randomly generated dataframe of MET and EX (mle$residuals)

ranGen <- function(data, mle){
  predEX <- predict(mle, newdata=data)
  residuals <- data$EX - predEX
  print(residuals)
  result <- data.frame(MET=data$MET, EX = rnorm(n=nrow(data), mean = predEX, sd(residuals)) )
  return(result)
   }

createPredictions <- function(data){
  treeModel <- tree(EX~MET, data=data, control=tree.control(nobs=nrow(data) , mincut=8) )
  modelTreePruned <- prune.tree(treeModel, best=3)
  predModel = predict(modelTreePruned, newdata=State)
  return(predModel)
}

predictModel <- function(data){
  pred <- createPredictions(data)
  return(rnorm(n=nrow(data), pred, sd(data$EX - pred)))
}

set.seed(12345)
#model confidence intervals

bootParametricModel <- boot(data=State, statistic=createPredictions, mle=fitTreePruned, ran.gen = ranGen, R=1000,
                           sim="parametric")

bootParaEnvelope <- envelope(bootParametricModel, level=0.95)

#data intervals

bootParametricData <- boot(data=State, statistic=predictModel, mle=fitTreePruned, ran.gen = ranGen, R=1000,
                           sim="parametric")
bootDataEnvelope <- envelope(bootParametricData, level=0.95)

plot(State$MET,State$EX , ylab="EX", xlab="MET", ylim=c(100,500), main="Parametric bootstrap")
points(State$MET, regrTree, col="red", type="b")
lines(State$MET, bootParaEnvelope$point[1,], col="grey", lty=2)
lines(State$MET, bootParaEnvelope$point[2,], col="grey", lty=2)
lines(State$MET, bootDataEnvelope$point[1,], col="grey", lty=3)
lines(State$MET, bootDataEnvelope$point[2,], col="grey", lty=3)
legend("bottomright", legend=c("Samples","Predictions",
                               "95% CI of predictions", "95% CI of data"),
       col=c(1,2,"grey","grey"), cex=.65, lty=c(NA,1,2,3), pch=c(1,1,NA,NA))

## the confidence interval is a lot smoother for the parametric bootstrap compared to nonparametric. 

