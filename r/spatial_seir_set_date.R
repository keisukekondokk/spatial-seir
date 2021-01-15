#ERROR CHECK
if(is.integer(numCaseScenario) == FALSE | numCaseScenario < 1 | numCaseScenario > 7) {
  stop("ERROR: Use integer from 1 to 7 to select one of the case scenarios.")
}
#
if(numCaseScenario == 1L){
  startDay <- as.Date("2020-04-07", "%Y-%m-%d")
} 
if(numCaseScenario == 2L){
  startDay <- as.Date("2020-04-07", "%Y-%m-%d")
}
if(numCaseScenario == 3L){
  startDay <- as.Date("2020-08-17", "%Y-%m-%d")
} 
if(numCaseScenario == 4L){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
}
if(numCaseScenario == 5L){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
} 
if(numCaseScenario == 6L){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
}
if(numCaseScenario == 7L){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
}
#
endDay <- as.Date("2023-12-31", "%Y-%m-%d")
dataDay <- as.Date("2021-01-11", "%Y-%m-%d") - (l_epsilon + l_gamma + 1)
