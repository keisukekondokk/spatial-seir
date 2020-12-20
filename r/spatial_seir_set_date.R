#
if(numCaseScenario == 1){
  startDay <- as.Date("2020-04-07", "%Y-%m-%d")
} 
if(numCaseScenario == 2){
  startDay <- as.Date("2020-04-07", "%Y-%m-%d")
}
if(numCaseScenario == 3){
  startDay <- as.Date("2020-08-17", "%Y-%m-%d")
} 
if(numCaseScenario == 4){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
}
if(numCaseScenario == 5){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
} 
if(numCaseScenario == 6){
  startDay <- as.Date("2020-11-04", "%Y-%m-%d")
}
#
endDay <- as.Date("2023-12-31", "%Y-%m-%d")
dataDay <- as.Date("2020-11-26", "%Y-%m-%d") - (l_epsilon + l_gamma + 1)
