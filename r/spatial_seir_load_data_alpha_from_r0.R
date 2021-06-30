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


##############
r0 <- 2.6
l_epsilon <- 5
##############

#Re National
dfRe <- readr::read_csv("data/parameter/source/effective_reproduction_number.csv")
dfRe <- dfRe %>%
  dplyr::rename(date = `日付`) %>%
  dplyr::rename(re = `実効再生産数`) %>%
  dplyr::mutate(date = lubridate::ymd(date) - l_epsilon) %>%
  dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::mutate(alpha = re / r0)

#Re by Prefecture
dfRePref <- readr::read_csv("data/parameter/source/prefectures.csv")
dfRePref <- dfRePref %>%
  dplyr::select(year, month, date, prefectureNameE, effectiveReproductionNumber) %>%
  dplyr::filter(prefectureNameE == "Tokyo") %>%
  dplyr::rename(day = date) %>%
  dplyr::mutate(date = lubridate::ymd(paste(year, month, day, sep = "-")) - l_epsilon) %>%
  dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::mutate(alpha = effectiveReproductionNumber/ r0) %>%
  dplyr::select(date, year, month, prefectureNameE, alpha)

#Alpha
dfAlpha0 <- dfRe %>%
  dplyr::filter(date >= as.Date("2020-06-01")) %>%
  dplyr::filter(date <= as.Date("2021-05-31")) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    month = first(month),
    alpha = mean(alpha, na.rm = TRUE)
  )

#Alpha
dfAlpha <- dfRePref %>%
  dplyr::filter(date >= as.Date("2020-06-01")) %>%
  dplyr::filter(date <= as.Date("2021-05-31")) %>%
  dplyr::group_by(prefectureNameE, month) %>%
  dplyr::summarise(
    month = first(month),
    prefectureNameE = first(prefectureNameE),
    alpha = mean(alpha, na.rm = TRUE)
  )

#Alpha for Tokyo
dfAlpha1 <- dfAlpha %>%
  dplyr::filter(prefectureNameE == "Tokyo") %>%
  dplyr::rename(alphaTokyo = alpha)

#Parameter for Simulation
dfParameter0 <- tidyr::crossing(dfPref, dfAlpha0) %>%
  dplyr::select(-prefNameJp, -prefNameEn)
dfParameter1 <- tidyr::crossing(dfPref, dfAlpha1) %>%
  dplyr::select(-prefNameJp, -prefNameEn, -prefectureNameE)

#Parameter
dfParameter <- dfParameter0 %>%
  dplyr::left_join(dfParameter1, by = c("prefCode", "month"))

#Parameter
dfParameterPref <- dfCalendarPref %>%
  dplyr::left_join(dfParameter, by = c("prefCode" = "prefCode", "date_month" = "month")) %>%
  dplyr::mutate(alpha = if_else(prefCode >= 10 & prefCode <= 14, alphaTokyo, alpha)) %>%
  dplyr::select(-alphaTokyo)

#Save
lapply(1:7, function(x){readr::write_csv(dfParameterPref, paste0("data/parameter/CSV_npis_degree_case", x, ".csv"))})
