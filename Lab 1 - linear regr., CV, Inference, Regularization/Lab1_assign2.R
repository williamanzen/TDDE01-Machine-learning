Dataframe_machines=read.csv2("machinescsv.csv")
n=dim(Dataframe_machines)[1]
set.seed(12345)

#From Likelihood-method
Loglikelihood <- function(x, theta){
  n <- length(x[,1])
  return(-n*log(theta)+theta*sum(x))
}

#from differentiating the Maximum Likelihood function and finding zero
ML_theta <- function(x){
n <- length(x[,1])
return(n/sum(x))
}

## Finding the peak likelihood value from the data
first <- curve( Loglikelihood(Dataframe_machines, x) , from=0, to=15 , add=FALSE ,ylim=c(0,160))
theta_one <- ML_theta(Dataframe_machines) # = 1.126217 x-"theta" for min -loglikelihood (peak point in theta)
six_values = t(Dataframe_machines[1:6,])


second <- curve(Loglikelihood(six_values,x), from=0, to=15 , add=TRUE , ylim=c(0,160) )
theta_two <- ML_theta(six_values)

#from log(PI(P(x|theta))*p(theta)) derived and break out theta-hat (where derivative is 0 break out theta)
L_theta <- function(x, theta, lambda){
  n <- length(x[,1])
  return(-(n*log(theta)-theta*sum(x)+log(lambda)-lambda*theta))
}

## Finding the peak of the posterior distribution calculated in L_theta
third <- curve(L_theta(Dataframe_machines, x, 10), from=0, to=4, add=FALSE,ylim=c(0,160) , col="red")
curve( Loglikelihood(Dataframe_machines, x) , from=0, to=4 , add=TRUE ,ylim=c(0,160))


calc_x_ML <- function(x, lambda){
n <- length(x[,1])
return(n/(lambda+sum(x)+log(lambda)))
}

calc_x_ML(Dataframe_machines,10) # = 0.8739483 min in x-"theta" -loglikelihood 

#rnorm rpois rexp different functions to random generate values around a certain value with certain dist.
fifty_r_values <- rexp(50, theta_one )

hist(fifty_r_values)
hist(Dataframe_machines$Length)
#båda histogramen är fördelade exponentiellt och visar att det är en rimlig distribution att anta. 
