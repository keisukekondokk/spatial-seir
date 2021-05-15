#Alpha (Degree of NPIs)
dfAlphaTemp <- dfCalendarPref %>%
  dplyr::left_join(listAlpha[[numCaseScenario]] %>% mutate(date = lubridate::ymd(date)) %>% select(date, prefCode, alpha), by = c("date", "prefCode")) %>%
  dplyr::filter(prefCode != 0) %>%
  dplyr::filter(date <= endDay) %>%
  dplyr::arrange(prefCode, date)
dfAlpha <- dfAlphaTemp %>%
  dplyr::filter(date <= endDay) %>%
  dplyr::mutate(alphaMa = zoo::rollmean(x = alpha, 21, na.pad = TRUE, align = "center")) %>%
  dplyr::mutate(alphaMa = if_else(is.na(alphaMa), alpha, alphaMa))
rm(dfAlphaTemp)

#COVID-19 Data
dfPanelDB <- dfPanel %>%
  dplyr::rename(N = pop) %>%
  dplyr::mutate(dI = newPositive) %>%
  dplyr::group_by(prefCode) %>%
  dplyr::mutate(numDays = row_number()) %>%
  dplyr::mutate(R = lag(cumPositive, n = l_gamma, defaut = 0)) %>%
  dplyr::mutate(I = cumPositive - R) %>%
  dplyr::mutate(E = lead(cumPositive, n = l_epsilon) - cumPositive) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(E = if_else(is.na(E) & date <= dataDay, 0, E)) %>%
  dplyr::mutate(I = if_else(is.na(I) & date <= dataDay, 0, I)) %>%
  dplyr::mutate(R = if_else(is.na(R) & date <= dataDay, 0, R)) %>%
  dplyr::mutate(S = N - E - I - R) %>%
  dplyr::mutate(S = if_else(date > dataDay, NA_real_, S)) %>%
  dplyr::mutate(E = if_else(date > dataDay, NA_real_, E)) %>%
  dplyr::mutate(I = if_else(date > dataDay, NA_real_, I)) %>%
  dplyr::mutate(R = if_else(date > dataDay, NA_real_, R)) %>%
  dplyr::mutate(dI = if_else(date > dataDay, NA_real_, dI)) %>%
  dplyr::select(numDays, everything())
rm(dfPanel)

#COVID-19 Data (Observed)
dfCovidPanel <- dfPanelDB %>%
  dplyr::filter(date <= dataDay)

#Snapshot at Starting Date of Simulation
dfSnapShotPref <- dfPanelDB %>%
  dplyr::filter(date == startDay) %>%
  dplyr::filter(prefCode != 0)

#Data from Stating Date of Simulation
dfSnapShotTime <- dfPanelDB %>%
  dplyr::filter(prefCode == 0) %>%
  dplyr::filter(date >= startDay & date <= endDay) %>%
  dplyr::mutate(rownumberPost = row_number()) %>% 
  dplyr::select(numDays, rownumberPost, date, date_year, date_month, date_day, date_holiday)

#Data from Stating Date of Simulation
dfCalendarAlpha <- dfSnapShotTime %>%
  dplyr::left_join(dfAlpha %>% dplyr::select(date, prefCode, alpha, alphaMa), by = "date") %>%
  dplyr::arrange(prefCode, date)

#Long Data before Starting Date of Simulation
dfPreLong <- dfPanelDB %>%
  dplyr::filter(date <= startDay) %>%
  dplyr::select(-newPositive, -cumPositive) %>%
  dplyr::select(numDays, date, prefCode, date_year, date_month, date_day, date_holiday, everything())

#Wide Data before Starting Date of Simulation
dfPreWide <- dfPreLong %>%
  tidyr::pivot_wider(id_col = c(numDays, date, date_year, date_month, date_day, date_holiday), 
                     names_from = prefCode, 
                     values_from = c(N, S, E, I, dI, R), 
                     names_sep = "") %>%
  dplyr::select(numDays, date, date_year, date_month, date_day, date_holiday, N0, S0, E0, I0, R0, dI0, everything())

#Panel
dfPreLongTest <- dfPreWide %>%
  tidyr::pivot_longer(-c(numDays, date, date_year, date_month, date_day, date_holiday),
                      names_to = c(".value", "prefCode"),
                      names_pattern = "(N|S|E|I|dI|R)(.*)") %>% 
  dplyr::arrange(prefCode, date)

#
dfPreLongTestNation <- dfPreLongTest %>%
  dplyr::filter(prefCode != 0) %>%
  dplyr::group_by(numDays, date, date_year, date_month, date_day, date_holiday) %>%
  dplyr::summarise(N = sum(N), 
                   S = sum(S),
                   E = sum(E),
                   I = sum(I),
                   dI = sum(dI),
                   R = sum(R)) %>%
  dplyr::mutate(prefCode = 0) %>%
  dplyr::ungroup()
rm(dfPreLongTest, dfPreLongTestNation)

#Setting for Simulation
region <- numPref
time <- nrow(dfSnapShotTime)
looptime <- time + 1

#Initial Values for Variables
vN <- dfSnapShotPref %>%
  dplyr::pull(N)
vE0 <- dfSnapShotPref %>%
  dplyr::pull(E)
vI0 <- dfSnapShotPref %>%
  dplyr::pull(I)
vdI0 <- dfSnapShotPref %>%
  dplyr::pull(dI)
vR0 <- dfSnapShotPref %>%
  dplyr::pull(R)
vS0 <- dfSnapShotPref %>%
  dplyr::pull(S)

#Matrix to Store Results
mN <- matrix(vN, looptime, region, byrow = TRUE)
mS <- matrix(NA_real_, looptime, region)
mE <- matrix(NA_real_, looptime, region)
mI <- matrix(NA_real_, looptime, region)
mdI <- matrix(NA_real_, looptime, region)
mR <- matrix(NA_real_, looptime, region)
mRatioLambda <- matrix(NA_real_, looptime, region)
mAlpha <- matrix(NA_real_, looptime, region)
colnames(mN) <- mapply(function(x) {paste0("N", x) }, 1:region)
colnames(mS) <- mapply(function(x) {paste0("S", x) }, 1:region)
colnames(mE) <- mapply(function(x) {paste0("E", x) }, 1:region)
colnames(mI) <- mapply(function(x) {paste0("I", x) }, 1:region)
colnames(mdI) <- mapply(function(x) {paste0("dI", x) }, 1:region)
colnames(mR) <- mapply(function(x) {paste0("R", x) }, 1:region)
colnames(mRatioLambda) <- mapply(function(x) {paste0("RatioLambda", x) }, 1:region)
colnames(mAlpha) <- mapply(function(x) {paste0("Alpha", x) }, 1:region)

#Initial Values
mS[1,] <- t(vS0)
mE[1,] <- t(vE0)
mI[1,] <- t(vI0)
mdI[1,] <- t(vdI0)
mR[1,] <- t(vR0)
