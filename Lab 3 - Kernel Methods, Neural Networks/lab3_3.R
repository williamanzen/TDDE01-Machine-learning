RNGversion('3.5.1')
#   install.packages("neuralnet")
#   library(neuralnet)
# library(ggplot2)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training 
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]

## 31 because 10 hidden layers, 1 depth, every node needs one bias, and one coefficient,
##end node needs 1 per node and 1 bias
winit <- runif(31, -1, 1)

trainMSE <- numeric()
valMSE <- numeric()
threshold <- numeric()
  for(i in 1:10) {
    nn <- neuralnet(Sin~Var , data=tr, startweights=winit, hidden=10, threshold=(i/1000))
    trainPred <- predict(nn, tr)
    valPred <- predict(nn, va)  
    
    threshold[i] <- i/1000
    
    trainMSE[i] <- mean((trainPred-tr$Sin)^2)
    valMSE[i] <- mean((valPred-va$Sin)^2)
    
  }

plot(threshold, valMSE, type="o",col="red", ylim=c(0.00009,0.001), xlim=c(0,0.01), ylab="MSE", xlab="Threshold", main="Validation set red - train set blue")


lines(threshold, trainMSE, type="o",col="blue")

plot(nn)
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1, col="black", main="prediction black - data red")
  points(trva, col = "red")
