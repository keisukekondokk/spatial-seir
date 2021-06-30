#Prefecture Code and Name
dfPref <- readr::read_csv("data/pref/CSV_list_pref_en.csv")
numPref <- nrow(dfPref)

#Population
dfPop <- readr::read_csv("data/pop/CSV_pop_by_pref_201910.csv")

#Calendar
dfCalendar <- readr::read_csv("data/calendar/CSV_calendar.csv") %>%
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d"))

#Calender by Prefecture
listPref <- lapply(0:numPref, function(x){ dfCalendar %>% dplyr::mutate(prefCode = x) })
dfCalendarPref <- bind_rows(listPref) 
rm(listPref)

#COVID-19 Infection Data by Prefecture
#Run "spatial_seir_load_data_from_nhk.R" beforehand
dfCovidPref <- readr::read_csv("data/covid19/df_nhk_news_covid19_prefectures_daily_data_2021-06-28.csv")

#COVID-19 Infection Data National Total
dfCovidNational <- dfCovidPref %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    date = first(date),
    newPositive = sum(newPositive),
    cumPositive = sum(cumPositive),
    newDeath = sum(newDeath),
    cumDeath = sum(cumDeath)
  ) %>%
  dplyr::mutate(prefCode = 0) %>%
  dplyr::mutate(prefName = "全国")

#COVID-19 Infection Data
dfCovid <- dplyr::bind_rows(dfCovidNational, dfCovidPref) %>%
  dplyr::arrange(prefCode, date)
dfPanel <- dfCalendarPref %>%
  dplyr::left_join(dfCovid, by = c("date" = "date", "prefCode" = "prefCode")) %>%
  dplyr::mutate(newPositive = if_else(date < as.Date("2020-01-16"), 0, newPositive)) %>%
  dplyr::mutate(cumPositive = if_else(date < as.Date("2020-01-16"), 0, cumPositive)) %>%
  dplyr::mutate(newDeath = if_else(date < as.Date("2020-01-16"), 0, newDeath)) %>%
  dplyr::mutate(cumDeath = if_else(date < as.Date("2020-01-16"), 0, cumDeath)) %>%
  dplyr::left_join(dfPref, by = "prefCode") %>%
  dplyr::mutate(prefNameJp = if_else(prefCode == 0, "全国", prefNameJp)) %>%
  dplyr::mutate(prefNameEn = if_else(prefCode == 0, "National Total", prefNameEn)) %>%
  dplyr::mutate(prefName = prefNameJp) %>%
  dplyr::left_join(dfPop %>% select(prefCode, pop), by = "prefCode") %>%
  dplyr::select(-prefNameJp, -prefNameEn)
rm(dfCovid, dfCovidNational, dfCovidPref)

#OD LIST across Prefectures
listSfFlow <- list()
listSfFlow[[1]] <- sf::read_sf("data/shp/SHP_lines_od_flows_by_pref_time14.shp")
listSfFlow[[2]] <- sf::read_sf("data/shp/SHP_lines_od_flows_by_pref_time20.shp")

#OD Matrix
listC <- list()
for(k in 1:2){
  listSfFlow[[k]] <- listSfFlow[[k]] %>%
    dplyr::rename(prefCodeOrigin = prefCdO) %>%
    dplyr::rename(prefCodeDestination = prefCdD) %>%
    dplyr::rename(prefNameOrigin = prefNmO) %>%
    dplyr::rename(prefNameDestination = prefNmD) %>%
    dplyr::rename(periodOfDay = prdOfDy) %>%
    dplyr::rename(periodOfTIme = prdOfTm) %>%
    dplyr::rename(totalPopOrigin = ttlPpOr)
  
  #OD Matrix from OD List
  dfFlow <- st_drop_geometry(listSfFlow[[k]])
  listC[[k]] <- list()
  for(i in 1:12) {
    listC[[k]][[i]] <- list()
    for(j in 1:2) {
      dfTemp <- dfFlow %>%
        dplyr::filter(month == i & periodOfDay == j) %>%
        dplyr::select(shFlow)
      matC <- matrix(dfTemp$shFlow/100, numPref, numPref, byrow = TRUE)
      listC[[k]][[i]][[j]] <- matC 
    }
    rm(dfTemp, matC)
  }
}
rm(dfFlow)

#Diagonal Matrix (no mobility across prefectures)
mIdentity <- diag(numPref)
