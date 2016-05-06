# plotType = 1 plots estimator bias. plotType = 2 plots variance of estimators.
# plotType = 3 plots quadratic error of estimators.
# estimatorType = 1 uses sigma_1 method (sum of quadratic error). estimatorType = 2 uses
# sigma_2 method (IQR method)
estimatorAssignment <- function(sampleSizes, meanValue, stdDev, iterations, plotType, estimatorType){
  
  # Vector keeping the average bias of each estimator by sample size
  estimatorBiasVector = c(0)
  # Vector keeping the average variance of each estimator by sample size
  estimatorVarianceVector = c(0)
  # Vector keeping the average quadratic error of each estimator by sample size
  estimatorQuadraticVector = c(0)
  
  for(s in 1:length(sampleSizes)){
    # Vector containing every estimator for given sample size
    estimatorSum = c(0)
    # Creates estimator for each iteration
    for(j in 1:iterations){
      sampleValues = rnorm(sampleSizes[s], mean = meanValue, sd = stdDev)
      sampleMean = mean(sampleValues)
      sampleSum = 0
      # Calculation of quadratic error inside estimator formula (sigma_1)
      for(x in 1:sampleSizes[s]){
        sampleSum = sampleSum + (sampleValues[x] - sampleMean)^2
      }
      if(estimatorType == 1){
        # Calculation of estimator for given iteration
        estimatorSum[j] = sqrt((1/(sampleSizes[s] - 1)) * sampleSum)
      }
      else{
        # Calculation of estimator (sigma_2)
        estimatorSum[j] = IQR(sampleValues)/1.349
      }
    }
    estimatorBiasVector[s] = mean(estimatorSum) - stdDev
    estimatorVarianceVector[s] = var(estimatorSum)
    estimatorQuadraticVector[s] = estimatorVarianceVector[s] + estimatorBiasVector[s]^2
  }
  
  # Variables for plotting graph
  lnValues = log(sampleSizes)
  mainTitle = "Sigma_1"
  if(estimatorType == 2){
    mainTitle = "Sigma_2"
  }
  
  # Plotting of bias of estimators
  if(plotType == 1){
    plot(lnValues, estimatorBiasVector, type = "b", main = mainTitle, xlab = "Log(n)", ylab = "Bias of estimator")
  }
  
  # Plotting of variance of estimators
  if(plotType == 2){
    lnVariances = log(estimatorVarianceVector)
    plot(lnValues, lnVariances, type = "b", main = mainTitle, xlab = "Log(n)",
         ylab = "Logarithm of variance of estimator")
  }
  
  # Plotting of quadratic error of estimators
  if(plotType == 3){
    lnQuad = log(estimatorQuadraticVector)
    plot(lnValues, lnQuad, type = "b", main = mainTitle, xlab = "Log(n)",
        ylab = "Logarithm of quadratic error of estimator")
  }
}