#Filename of Observed Data
saveFileName0 <-
  paste0(
    "output/csv/case",
    sprintf("%02d", numCaseScenario),
    "/CSV_covid19_panel_by_pref.csv"
  )

#Filename of Simulated Data
if(numCaseScenario == 1){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_without_mobility.csv"
    )
}
if(numCaseScenario == 2){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_without_mobility.csv"
    )
}
if(numCaseScenario == 3){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_at_8pm.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_at_2pm.csv"
    )
}
if(numCaseScenario == 4){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_except_I.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
}
if(numCaseScenario == 4){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_except_I.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
}
if(numCaseScenario == 5){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_except_Greater_Tokyo_Area.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
}
if(numCaseScenario == 6){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_except_Greater_Osaka_Area.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
}
if(numCaseScenario == 7){
  #
  saveFileName1 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility_except_Tokyo_Osaka.csv"
    )
  saveFileName2 <-
    paste0(
      "output/csv/case",
      sprintf("%02d", numCaseScenario),
      "/CSV_simulation_with_mobility.csv"
    )
}


#Save Panel Data and Results
readr::write_csv(dfPanelDB, saveFileName0)
readr::write_csv(listSimulationResults[[1]], saveFileName1)
readr::write_csv(listSimulationResults[[2]], saveFileName2)
