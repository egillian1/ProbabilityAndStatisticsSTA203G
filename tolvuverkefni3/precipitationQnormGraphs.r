
# Parameter logarithm is a boolean variable to determine
# whether logarithmic data is used

precipitationQnormGraphs <- function(logarithm){
  
  # Input of data for statistic processing
  max_urkoma <- read.table("precipitation_fagurhm.txt")
  log_urkoma <- log(sort(max_urkoma[,2]))
  
  # Use logarithmic data if requested
  if(logarithm){
    message("Notice: Logarithmic data being used")
  }
  
  # Often used statistics
  sortedData <- sort(max_urkoma[,2])
  meanOfData <- mean(sortedData)
  stdDevOfData <- sd(sortedData)
  n <- length(sortedData)
  logMeanOfData <- mean(log_urkoma)
  logStdDevOfData <- sd(log_urkoma)
  
  # Produce values according to assignment and plot them
  if(logarithm){
    functionValues <<- exp(logMeanOfData + logStdDevOfData*qnorm((1:n)/(n+1)))
  }else{
    functionValues <<- meanOfData + stdDevOfData*qnorm((1:n)/(n+1))
  }
  plot(sortedData, functionValues, type = "n", xlab = "precipitation(mm)", ylab = "function values")
  lines(sortedData, functionValues)
  lines(c(0,max(functionValues)), c(0,max(functionValues)))
  }