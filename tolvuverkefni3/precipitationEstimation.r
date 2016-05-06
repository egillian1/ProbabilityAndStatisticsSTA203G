
# Various estimations of input data

precipitationEstimation <- function(amount){
  
  # Input of data for statistic processing
  max_urkoma <- read.table("precipitation_fagurhm.txt")
  
  # Often used statistics
  sortedData <<- sort(max_urkoma[,2])
  meanOfData <- mean(sortedData)
  stdDevOfData <- sd(sortedData)
  n <- length(max_urkoma[,2])
  
  # Find place in data where values are less than amount
  place = 1
  for(g in 1:n){
    if(sortedData[g] > amount){
      break
    }
    place <- g
  }
  
  # Calculate probability built on proportions
  proportion <- (n - place)/n
  message("Probability of more than ", amount, 
          " mm of precipitation built on proportions: ", proportion)
  
  # Calculate the probability built on normal distribution
  proportion <- 1 - pnorm(amount, mean = meanOfData, sd = stdDevOfData)
  message("Probability of more than ", amount, 
          " mm of precipitation built on normal distribution: ", proportion)
  
  # Calculate the probability built on log-normal distribution
  proportion <- 1 - pnorm(log(amount), mean = mean(log(sortedData)), sd = sd(log(sortedData)))
  message("Probability of more than ", amount, 
          " mm of precipitation built on log-normal distribution: ", proportion)
  
}