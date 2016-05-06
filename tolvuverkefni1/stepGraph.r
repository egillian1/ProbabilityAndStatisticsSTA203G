stepGraph <- function(iterations, stepcount) {
  
  # Variables
  leftstep = -0.8
  rightstep = 1.3
  # Time at which each step is taken (y-axis)
  timecounter = c(0:(stepcount-1))
  # Initialization of graph
  plot(timecounter, timecounter, type = "n",
       xlab = "displacement", ylab = "time")
  
  for (g in 1:iterations) {
    # Vector containing current distance at each step
    stepdata = c(0)
    for (h in 2:stepcount) {
      if(rbinom(1,size=1,prob=0.82)) {
        stepdata[h] = stepdata[h-1] + rightstep
      }
      else {
        stepdata[h] = stepdata[h-1] + leftstep
      }
    }
    lines(stepdata, timecounter, type = "l")
  }
}