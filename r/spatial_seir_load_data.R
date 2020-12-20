#Prefecture Code and Name
dfPref <- readr::read_csv("data/pref/CSV_list_pref_en.csv")
numPref <- nrow(dfPref)

#Population
dfPop <- readr::read_csv("data/pop/CSV_pop_by_pref_201910.csv")

#Calendar
dfCalendar <- readr::read_csv("data/calendar/CSV_calendar.csv") %>%
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d"))

#COVID-19 Infection Data by Prefecture
dfCovid <- readr::read_csv("data/covid19/df_covid19_2020-11-26.csv") %>%
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d"))
dfPanel <- dfCovid %>%
  dplyr::left_join(dfPop %>% select(prefCode, pop), by = "prefCode")

#OD LIST across Prefectures
sfFlow <- sf::read_sf("data/shp/SHP_lines_od_flows_by_pref.shp")
sfFlow <- sfFlow %>%
  dplyr::rename(prefCodeOrigin = prefCdO) %>%
  dplyr::rename(prefCodeDestination = prefCdD) %>%
  dplyr::rename(prefNameOrigin = prefNmO) %>%
  dplyr::rename(prefNameDestination = prefNmD) %>%
  dplyr::rename(periodOfDay = prdOfDy) %>%
  dplyr::rename(periodOfTIme = prdOfTm) %>%
  dplyr::rename(totalPopOrigin = ttlPpOr)

#OD Matrix from OD List
dfFlow <- st_drop_geometry(sfFlow)
listC <- list()
for(i in 1:12) {
  listC[[i]] <- list()
  for(j in 1:2) {
    dfTemp <- dfFlow %>%
      dplyr::filter(month == i & periodOfDay == j) %>%
      dplyr::select(shFlow)
    matC <- matrix(dfTemp$shFlow/100, 47, 47, byrow = TRUE)
    listC[[i]][[j]] <- matC 
  }
  rm(dfTemp, matC)
}

#Diagonal Matrix (no mobility across prefectures)
mIdentity <- diag(numPref)
