poisProblem <- function(iterations, time, c, graphNum){
  
  # Vector with event counter for each iteration
  eventCount = c(0)
  # Vector with occurrence times of events for each iteration
  eventTimes = c(0)
  
  # Outer loop for generating iterations
  for(h in 1:iterations){
    # Events occured this iteration
    tmpTime = 0
    tmpEvents = 0
    while(tmpTime < time){
      tmpTime = tmpTime + rexp(1,rate = c)
      if(tmpTime < time){
        tmpEvents = tmpEvents + 1
      }
      # Add time of event to vector for graph purposes
      if(h <= graphNum){
        eventTimes = c(eventTimes, tmpTime)
      }
    }
    # NA used as separator between graph lines
    if(h <= graphNum){
      eventTimes = c(eventTimes, NA)
    }
    eventCount[h] = tmpEvents
  }
  
  # Export variables to global scale for debugging purposes
  eventCount <<- eventCount
  eventTimes <<- eventTimes
  
  # Parameter for simulated Poisson Distribution
  lambda = c*time
  
  # Likelihood of 0:13 events occurring calculated
  likelyVector = dpois(c(0:13),lambda,log = FALSE)
  likelyVector <<- likelyVector
  for(j in 1:14){
    likelyVector[j] = round(likelyVector[j], 4)
  }
  
  # Check how often events have occurred in simulation with given frequency 0 to 13
  densityCounter = rep(0,14)
  for(k in 1:14){
    for(l in 1:iterations){
      if(eventCount[l] == k-1){
        densityCounter[k] = densityCounter[k] + 1
      }
    }
    densityCounter[k] = round(densityCounter[k] / iterations, 4)
  }
  
  # Initialize plot for graphs
  plot(c(0, time), c(0, graphNum), type = "n", xlab = "time", ylab = "iteration")
  
  # Remove first point (false value)
  eventTimes = eventTimes[2:length(eventTimes)]
  # Graph lines and occurrences
  tmp = graphNum
  while(tmp > 0){
    lines(c(0,time),c(tmp,tmp))
    placeholder = 1;
    # Keep graphing same line until too big value is hit or NA value is hit (NA used as separator)
    while(!is.na(eventTimes[placeholder]) & eventTimes[placeholder] <= time){
      points(eventTimes[placeholder], tmp)
      placeholder = placeholder + 1
    }
    # Cut off all values that are "in front of" NA and throw them away
    eventTimes = eventTimes[placeholder + 1: length(eventTimes)]
    tmp = tmp - 1
  }
  
  message("Mean value of events occurring in ", time , " units of time: ", mean(eventCount))
  message("Theoretical expected value of events to occur in ", time, " units of time: ", lambda)
  message("Standard deviation of events occurring in ", time, " units of time: ", sd(eventCount))
  message("Theoretical standard deviation of events occurring in ", time, " units of time: ", sqrt(lambda))
  message("Likelihood of 0 to 13 events occurring with given Poisson distribution: ")
  cat(likelyVector, sep = " ")
  message("Percentage of 0 to 13 events occured in simulation with given Poisson distribution: ")
  cat(densityCounter, sep = " ")
  
  }