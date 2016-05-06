confidenceIntervalAssignment <- function(sampleSize, meanValue, stdDev, iterations){
  
  # 31. quantile for normal distribution
  quantileValue = meanValue + qnorm(1-0.31) * stdDev
  # Counter keeps track of how many samples are within safety interval
  counter = 0
  
  for(i in 1:iterations){
    # Generate random normal values with parameters
    normalValues = rnorm(sampleSize, meanValue, stdDev)
    # Quadratic error for given sample
    quadError = 0
    for(s in 1:sampleSize){
      quadError = quadError + (normalValues[s] - meanValue)^2
    }
    # Calculate estimator for given sample
    #estimator = sqrt((1/(sampleSize - 1)) * quadError)
     estimator = sd(normalValues)
    # Calculate confidence interval for given sample
    middleValue = mean(normalValues) + qnorm(0.69) * estimator
    shiftValue = qnorm(0.025)*(sqrt(estimator^2 + 0.5*qnorm(0.69)^2*estimator^2)/sqrt(sampleSize))
    confidenceUpper = middleValue + abs(shiftValue)
    confidenceLower = middleValue - abs(shiftValue)
    # Check if confidence interval contains quantile value
    if(confidenceLower <= quantileValue && quantileValue <= confidenceUpper){
      counter = counter + 1
    }
  }
  
  message((counter/iterations)*100)
}