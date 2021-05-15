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

#Re
r0 <- 2.6
l_epsilon <- 5
dfRe <- readr::read_csv("data/parameter/source/effective_reproduction_number.csv")
dfRe <- dfRe %>%
  dplyr::rename(date = `日付`) %>%
  dplyr::rename(re = `実効再生産数`) %>%
  dplyr::mutate(date = lubridate::ymd(date) - l_epsilon) %>%
  dplyr::mutate(alpha = re / r0)

#Re by Prefecture
dfRePref <- readr::read_csv("data/parameter/source/prefectures.csv")
dfRePref <- readr::read_csv("data/parameter/source/prefectures.csv", col_types = list("RatioLambda" = "d", "Alpha" = "d"))
dfRePref <- dfRePref %>%
  dplyr::select(year, month, date, prefectureNameE, effectiveReproductionNumber) %>%
  dplyr::filter(prefectureNameE == "Tokyo") %>%
  dplyr::rename(day = date) %>%
  dplyr::mutate(date = lubridate::ymd(paste(year, month, day, sep = "-")) - l_epsilon) %>%
  dplyr::mutate(alpha = effectiveReproductionNumber/ r0) %>%
  dplyr::select(date, year, month, alpha)

#Alpha
dfAlpha <- dfRePref %>%
  dplyr::filter(date >= as.Date("2020-06-01")) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    month = first(month),
    alpha = mean(alpha)
  )
  
#Parameter for Simulation
dfParameter <- tidyr::crossing(dfPref, dfAlpha) %>%
  dplyr::select(-prefNameJp, -prefNameEn)

#
dfParameterPref <- dfCalendarPref %>%
  dplyr::left_join(dfParameter, by = c("prefCode" = "prefCode", "date_month" = "month"))

#Save
lapply(1:7, function(x){readr::write_csv(dfParameterPref, paste0("data/parameter/CSV_npis_degree_case", x, ".csv"))})
