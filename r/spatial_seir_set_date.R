#ERROR CHECK
if(is.integer(numCaseScenario) == FALSE | numCaseScenario < 1L | numCaseScenario > numTotalScenario) {
  stop(paste0("ERROR: Use integer from 1 to ", numTotalScenario, " to select one of the case scenarios."))
}
#
if(numCaseScenario == 1L){
  #Calibration
  startDay <- as.Date("2020-04-07", "%Y-%m-%d")
} 
if(numCaseScenario == 2L){
  #Projection
  startDay <- as.Date("2021-04-23", "%Y-%m-%d")
}
if(numCaseScenario == 3L){
  #Projection at 8pm
  startDay <- as.Date("2021-04-23", "%Y-%m-%d")
} 
if(numCaseScenario == 4L){
  #Projection Infectious Individuals
  startDay <- as.Date("2021-04-23", "%Y-%m-%d")
}
if(numCaseScenario == 5L){
  #Projection Greater Tokyo Area
  startDay <- as.Date("2021-04-23", "%Y-%m-%d")
} 
if(numCaseScenario == 6L){
  #Projection Greater Osaka Area
  startDay <- as.Date("2021-04-23", "%Y-%m-%d")
}
if(numCaseScenario == 7L){
  #Projection Tokyo and Osaka
  startDay <- as.Date("2021-04-23", "%Y-%m-%d")
}

#
endDay <- as.Date("2023-12-31", "%Y-%m-%d")
dataDay <- as.Date("2021-05-09", "%Y-%m-%d") - (l_epsilon + l_gamma + 1)
