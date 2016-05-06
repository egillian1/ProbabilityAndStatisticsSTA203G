stepProblem <- function(totalstepcount, iterations) {
  
  # Variables
  leftstep = -0.8
  rightstep = 1.3
  # Vector keeping data for each run
  rundata = c(0)

  # Total distance travelled during all runs
  totaldistance = 0
  for (h in 1:iterations){
    # Total distance travelled during this run
    rundistance = 0;
    for (i in 1:totalstepcount) {
      if(rbinom(1,size=1,prob=0.82)) {
        rundistance = rundistance + rightstep
      }
      else {
        rundistance = rundistance + leftstep
      }
    }
    # Data for this run moved into vector of total distance
    rundata[h] = rundistance
    totaldistance = totaldistance + rundistance
  }
  
  # Data for total distance exported as variable on global scope
  rundata <<- rundata
  
  message("Total distance: ", totaldistance)
  message("Mean distance: ", mean(rundata))
  message("Standard deviation of distance: ", sd(rundata))
}
