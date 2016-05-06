
" Variable graphType is an integer and used for determining what method should be 
implemented in creating the graph/plot (0 for scatter graph, 1 for box plot,
2 for histogram, 3 for cumulative distribution function or 4 for quantile function).
The variable logarithm is a boolean value to determine whether the logarithm of
the data should be used or not (TRUE means logarithmic values are used)."

precipitationGraphs <- function(graphType,logarithm){
  
  # Input of data for statistic processing
  max_urkoma <- read.table("precipitation_fagurhm.txt")
  
  # Use logarithmic data if requested
  if(logarithm){
    max_urkoma[2] <- log(max_urkoma[2])
    message("Notice: Logarithmic data being used")
  }
  
  # Often used statistics
  sortedData <- sort(max_urkoma[,2])
  meanOfData <<- mean(sortedData)
  stdDevOfData <<- sd(sortedData)
  n <- length(max_urkoma[,2])
  
  # Draw scatter graph if required (graphType = 0)
  if(graphType == 0){
    plot(max_urkoma, xlab="year", ylab="precipitation(mm)")
    # Check what year the precipitation was the most
    maxRow = 1
    for(g in 1:n){
      if(max_urkoma[g,2] > max_urkoma[maxRow,2]){
        maxRow <- g
      }
    }
    message("Maximum precipitation was ", max_urkoma[maxRow,2], " in the year ", max_urkoma[maxRow,1])
  }
  
  # Draw boxplot if required (graphType = 1)
  if(graphType == 1){
    boxplotInstance <- boxplot(max_urkoma[2], ylab="precipitation(mm)")
    message("Number of outliers in boxplot: ", length(boxplotInstance$out))
    }
  
  # Draw histogram if required (graphType = 2)
  if(graphType == 2){
    precipitation <- max_urkoma[,2]
    histogramInstance <- hist(precipitation, plot = F)
    yLimit <- range(0,histogramInstance$density, dnorm(0)/sd(precipitation))
    hist(precipitation, freq = F, ylim = yLimit, ylab = "frequency")
    curve(dnorm(x, mean = mean(max_urkoma[,2]), sd = sd(max_urkoma[,2])), add = T)
  }
  
  # Draw cumulative distribution function if required (graphType = 3)
  if(graphType == 3){
    # Cumulative distribution function for data
    plot(sortedData, (1:n)/n, type = "s", ylim = c(0,1), xlab = "precipitation(mm)", ylab = "probability")
    # Cumulative distribution function based on statistics from data
    curve(pnorm(x, mean = meanOfData, sd = stdDevOfData), add = T)
    }
  
  # Draw quantile function id required (graphType = 4)
  if(graphType == 4){
    # Quantile function for data
    sortedData <- sort(max_urkoma[,2])
    p <- (1:n)/(n + 1)
    plot(p,sortedData, type = "s", xlim = c(0,1), xlab = "probability", ylab = "precipitation(mm)")
    # Quantile function built on data statistics
    if(logarithm){
      # Workaround for logarithmic graph
      par(new = TRUE)
      curve(qnorm(x,mean = meanOfData, sd = stdDevOfData), xlab = "", ylab = "")
    }else{
      curve(qnorm(x,mean = meanOfData, sd = stdDevOfData), add = T) 
    }
  }
}