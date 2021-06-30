#url <- "https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv"

dfCovidPref <- readr::read_csv("data/covid19/nhk_news_covid19_prefectures_daily_data.csv") %>%
  dplyr::rename(date = `日付`) %>%
  dplyr::rename(prefCode = `都道府県コード`) %>%
  dplyr::rename(prefName = `都道府県名`) %>%
  dplyr::rename(newPositive = `各地の感染者数_1日ごとの発表数`) %>%
  dplyr::rename(cumPositive = `各地の感染者数_累計`) %>%
  dplyr::rename(newDeath = `各地の死者数_1日ごとの発表数`) %>%
  dplyr::rename(cumDeath = `各地の死者数_累計`) %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::mutate(prefCode = as.numeric(prefCode)) %>%
  dplyr::arrange(prefCode, date)

#
date_download <- "2021-06-28"

#
readr::write_csv(dfCovidPref, paste0("data/covid19/df_nhk_news_covid19_prefectures_daily_data_", date_download, ".csv"))
