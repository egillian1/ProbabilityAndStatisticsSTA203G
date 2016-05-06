logProblem <- function(mew, stdev, iterations, days, numgraphs) {
  
  # Vector containing prices of shares after calculations
  sharesprices = c(0)
  # Matrix containing vectors with y positional values of shares prices
  positions = matrix(data = NA, nrow = numgraphs, ncol=days, byrow = T)
  # Values for containing maximum and minimum prices for graph scaling
  maxPrice = 1;
  minPrice = 1;
  
  # Perform number of iterations specified and calculate final prices
  for(h in 1:iterations){
    # Calculate the final price of the shares for given days
    finalPrice = 0;
    # Vector for keeping share price on each day for graph purposes
    sharePosition = c(0)
    for(i in 1:days){
      finalPrice = finalPrice + rnorm(1,mean=mew,sd=stdev)
      sharePosition[i] = exp(finalPrice)
      if(h < numgraphs + 1){
        if(sharePosition[i] > maxPrice){
          maxPrice = sharePosition[i];
        }
        if(sharePosition[i] < minPrice){
          minPrice = sharePosition[i]
        }
      }
    }
    sharesprices[h] = finalPrice
    # Add points to matrix for graphing purposes
    if(h < numgraphs + 1){
      positions[h, ] <- sharePosition
    }
  }
  
  # Calculated real values of shares
  realMean = mean(sharesprices)
  realDev = sqrt(exp(2*realMean + var(sharesprices)))
  
  # Calculate theoretical mean price and standard deviation of shares on logarithmic scale
  theorMean = mew * days
  theorDev = sqrt(stdev^2 * days)
  theorRealDev = sqrt(exp(2*theorMean + theorDev^2))
  
  message("Logarithmic mean price of shares after ", days, " days: ", realMean)
  message("Real-scale mean price of shares after ", days, " days: ", exp(realMean))
  message("Logarithmic standard deviation of first share after ", days, " days: ", sd(sharesprices))
  message("Real-scale standard deviation of first share after ", days, " days: ", realDev)
  message("______________________________________________________")
  message("Theoretical logarithmic mean price of shares after ", days, " days: ", theorMean)
  message("Theoretical real-scale mean price of shares after ", days, " days: ", exp(theorMean))
  message("Theoretical logarithmic standard deviation of shares after ", days, " days: ", theorDev)
  message("Theoretical real-scale standard deviation of shares after ", days, " days: ", theorRealDev)
  
  # Export positions matrix to global scale for debugging
  positions <<- positions
  
  # Offset min and max values so no clipping occurs on graph
  maxPrice = maxPrice + 0.01
  minPrice = minPrice - 0.01
  
  # Initialize graph for shares
  plot(c(0, days), c(minPrice, maxPrice), type = "n", xlab = "days", ylab = "price")
  
  # Plot set number of shares-price graphs
  for(j in 1:numgraphs){
    lines(positions[j, ], type="l", col= (83 + j*5) %% 657)
  }
}