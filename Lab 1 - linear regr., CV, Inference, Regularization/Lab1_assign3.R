#linear regression
linRegr <- function(X, Y, Xpred){
  X <- cbind(1, X)
  # beta estimation
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  beta <- signif(beta, digits=5)
  # linear regression
  Res <- cbind(1,Xpred) %*% beta
  return(Res)
}

## Perform N-fold Cross Validation with Linear Regression
## INPUTS:
## X - feature variables
## Y - target variables
## Nfolds - Number of folds
myCV <- function(X, Y, Nfolds) {
  set.seed(12345)
  n <- length(Y) # number of samples
  p <- ncol(X) # dimension of sample
  ind <- sample(n, n) # random permutation
  X1 <- X[ind,]
  Y1 <- Y[ind]
  sF <- floor(n/Nfolds) # samples per fold
  MSE <- numeric(2^p-2) 
  Nfeat <- numeric(2^p-2)
  Features <- list()
  
  folds <- cut(seq(1,nrow(X1)),breaks=Nfolds,labels=FALSE)
  
  
  for (i in 1:30) {
    # creates combinations of train & test
    modelBin <- sapply(i, function(x) { as.integer(intToBits(x))})[1:5]
    model <- c(rep(modelBin, each=sF), rep(modelBin[length(modelBin)], n-sF*Nfolds))
    
    # split the data
    testIndices <- which(model == T)
    trainData_X <- X1[-testIndices,]
    trainData_Y <- Y1[-testIndices]
    testData_X <- X1[testIndices,]
    testData_Y <- Y1[testIndices]
    
    # evaluate linear regression
    testResults <- linRegr(trainData_X, trainData_Y, testData_X)
    
    # compute errors
    SE <- (testResults - testData_Y)^2
    SSE <- sum(SE)
    tableSE <- cbind(testData_Y, testResults, SE)
    
    # report the results
    MSE[i] <- SSE/length(testData_X)
    Nfeat[i] <- sum(modelBin)
    Features[[i]] <- modelBin
  }
  
  plot(MSE ~ Nfeat)
  
  # return the best
  i <- which.min(MSE)
  return(list(
    CV=MSE[i],
    Features=Features[[i]])
  )
  
}

myCV(as.matrix(swiss[,2:6]), swiss[[1]], 5)