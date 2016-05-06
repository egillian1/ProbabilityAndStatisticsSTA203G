precipitationStats <- function() {
  
  # Input of data for statistic processing
  max_urkoma <- read.table("precipitation_fagurhm.txt")
  urkoma <- max_urkoma[,2]
  # Create matrix for storing statistics and label columns/rows
  statMatrix <- matrix(rep(c(0),7),ncol=7,byrow=TRUE)
  colnames(statMatrix) <- c("Mean", "Median", "Variance", "Standard Deviation",
                            "First Quartile", "Third Quartile", "Interquartile Range")
  rownames(statMatrix) <- c("Value")
  
  # Calculate statistical values and input data
  statMatrix[1,1] <- mean(urkoma)
  statMatrix[1,2] <- median(urkoma)
  statMatrix[1,3] <- var(urkoma)
  statMatrix[1,4] <- sd(urkoma)
  statMatrix[1,5:6] <- quantile(urkoma,probs = c(0.25,0.75))
  statMatrix[1,7] <- statMatrix[1,6] - statMatrix[1,5]
  
  # Export table to global scope for debugging
  statMatrix <<- statMatrix
  print(statMatrix)
  
  # Check if mean or median value is bigger
  if(statMatrix[1,1] > statMatrix[1,2]){
    message("Mean value is larger than median value.")
  } else if(statMatrix[1,1] < statMatrix[1,2]){
    message("Median value is larger than mean value.")
  } else{
    message("Mean value and median value are equal.")
  }
  
  # Calculate range for 50 percent of values with median values as middle point
  message("Range with 50 percent of values with median as mid-point: [",
          statMatrix[1,5], " ; ", statMatrix[1,6], "]")
  
  
  ######################
  # LOGARITHM STATISTICS
  ######################
  
  
  # Calculate logarithmic values of data
  logUrkoma <- log(urkoma)
  # Create matrix for storing statistics and label columns/rows
  logStatMatrix <- matrix(rep(c(0),7),ncol=7,byrow=TRUE)
  colnames(logStatMatrix) <- c("Mean", "Median", "Variance", "Standard Deviation",
                            "First Quartile", "Third Quartile", "Interquartile Range")
  rownames(logStatMatrix) <- c("Logarithmic Values")
  
  # Calculate statistical values and input logarithmic data
  logStatMatrix[1,1] <- mean(logUrkoma)
  logStatMatrix[1,2] <- median(logUrkoma)
  logStatMatrix[1,3] <- var(logUrkoma)
  logStatMatrix[1,4] <- sd(logUrkoma)
  logStatMatrix[1,5:6] <- quantile(logUrkoma,probs = c(0.25,0.75))
  logStatMatrix[1,7] <- logStatMatrix[1,6] - logStatMatrix[1,5]
  
  # Export table to global scope for debugging
  logStatMatrix <<- logStatMatrix
  print(logStatMatrix)
}