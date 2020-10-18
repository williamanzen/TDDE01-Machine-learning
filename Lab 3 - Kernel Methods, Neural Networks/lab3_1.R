RNGversion('3.5.1')
set.seed(123456)
 # install.packages("geosphere")
 # library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

## Predicting temperatures with a kernel method

  # These three values are up to the students
  h_distance <- 500000
  h_date <- 10
  h_time <- 4
    
  # The point to predict (up to the students)
  a <- 58.4274
  b <- 14.826
  coordsVector<- c(b,a)

  
date <- "2013-06-04" # The date to predict (up to the students)


times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00"
           , "18:00:00", "20:00:00", "22:00:00", "24:00:00")


# temp <- vector(length=length(times))


## FILTER AWAY DATES EARLIER THAN
filterDate <- function(dataset, date){
  return(dataset[!as.Date(dataset$date) >= as.Date(date),])
}

stDates <- filterDate(st, date) # updated to only dates before set date


## DISTANCE CALCULATIONS
kernelVectorDist <- c()
## Calculate distance impact
for(i in 1:length(stDates[,1])){
distVector <- c(stDates$longitude[i], stDates$latitude[i])
kernelVectorDist <- c(kernelVectorDist, exp(-(distHaversine(coordsVector,distVector)/h_distance)^2))
}


## TIME CALCULATIONS
calcDayDiff <- function(measuredDate, date){
  measured = (paste("2000",substr(measuredDate,5,10),sep=""))
  ourDate = (paste("2000",substr(date,5,10),sep=""))
  diffTime = difftime(measured, ourDate, units=c("days"))
  diff = as.numeric(diffTime,units="days")
  
  if(diff>=183){
    diff= 365 - diff
  }
  return(diff)
}
## Calculate Day difference
kernelVectorDates <- c()
for(i in 1:length(stDates[,1])){
  kernelVectorDates <- c(kernelVectorDates, exp(-(calcDayDiff(stDates$date[i], date)/h_date)^2))
}

## HOUR CALCULATIONS
calcTimeDiff <- function(hourOne, hourTwo){
  hourOne= as.numeric(substr(hourOne,1,2))
  hourTwo= as.numeric(substr(hourTwo,1,2))
  hourDiff = abs(hourOne-hourTwo)
  if(hourDiff>12){
    hourDiff = 24 - hourDiff
  }
  return(hourDiff)
}


kernelTime <- function(obsHour, curHour){
  return(exp(-(calcTimeDiff(obsHour, curHour)/h_time)^2))
}

## Sum/multiply the three kernel values to estimate the predicted values
## And compare sum vs multiplications
## Inputs:
## kernelDist - kernel Distance
## kernelDate - kernel date
## date - date to predict
## dataset - previous data
kernelSum <- function(kernelDist, kernelDate, dataset, date){
  
  tempVectorMult <- c()
  tempVectorAdd <- c()
  

  
  for(z in 1:length(times)){
    tempMult = 0
    tempMultTop = 0
    
    tempSum = 0
    tempSumTop = 0
    for(i in 1:length(dataset[,1])){
      
      kernelTempTime = kernelTime(dataset$time[i],times[z])
  
      kernelMult = kernelTempTime*kernelVectorDates[i]*kernelVectorDist[i]
      
      kernelAdd = kernelTempTime+kernelVectorDates[i]+kernelVectorDist[i]
      
      tempMultTop = tempMultTop + kernelMult*dataset$air_temperature[i]
      tempMult = tempMult + kernelMult

      tempSumTop = tempSumTop + kernelAdd*dataset$air_temperature[i]
      tempSum = tempSum + kernelAdd
    }
    tempVectorMult = c(tempVectorMult, tempMultTop/tempMult)
    tempVectorAdd = c(tempVectorAdd, tempSumTop/tempSum)    
    
  }
  
  return(cbind(tempVectorMult, tempVectorAdd))
}

kernelVectors <- kernelSum(kernelVectorDist, kernelVectorDates, stDates, date)

plot(kernelVectors[,1], type="o", main="multiplication")
plot(kernelVectors[,2], type="o", main="addition")

