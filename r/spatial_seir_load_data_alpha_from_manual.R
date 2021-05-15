#Prefecture Code and Name
dfPref <- readr::read_csv("data/pref/CSV_list_pref_en.csv")
numPref <- nrow(dfPref)

#Calendar
dfCalendar <- readr::read_csv("data/calendar/CSV_calendar.csv") %>%
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d"))

#Calender by Prefecture
listPref <- lapply(0:numPref, function(x){ dfCalendar %>% dplyr::mutate(prefCode = x) })
dfCalendarPref <- bind_rows(listPref) %>%
  dplyr::filter(prefCode != 0)
rm(listPref)

#Alpha
dfAlpha <- readr::read_csv("data/parameter/source/CSV_npis_degree_case1_year_month.csv")

#Parameter for Simulation
dfParameter <- tidyr::crossing(dfPref, dfAlpha) %>%
  dplyr::select(-prefNameJp, -prefNameEn)

#
dfParameterPref <- dfCalendarPref %>%
  dplyr::left_join(dfParameter, by = c("prefCode" = "prefCode", "date_year" = "year", "date_month" = "month"))


#Save
lapply(1:7, function(x){readr::write_csv(dfParameterPref, paste0("data/parameter/CSV_npis_degree_case", x, ".csv"))})
