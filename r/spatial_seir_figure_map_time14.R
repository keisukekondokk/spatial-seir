#+++++++++++++++
# Load Data
#+++++++++++++++


#Prefecture Code and Name
dfPref <- readr::read_csv("data/pref/CSV_list_pref_en.csv")
numPref <- nrow(dfPref)

#Population
dfPop <- readr::read_csv("data/pop/CSV_pop_by_pref_201910.csv")

#Shapefile
sfPref <- sf::read_sf("data/shp/SHP_japan_pref.shp", options = "ENCODING=UTF-8")
sfPref <- sfPref %>%
  dplyr::left_join(dfPref, by = "prefCode")

#Greater Tokyo Area
sfPrefTokyo <- sfPref %>%
  dplyr::filter(prefCode >= 11 & prefCode <= 14) %>%
  sf::st_union()
#Greater Nagoya Area
sfPrefNagoya <- sfPref %>%
  dplyr::filter(prefCode== 21 | prefCode == 23 | prefCode == 24) %>%
  sf::st_union()
#Greater Osaka Area
sfPrefOsaka <- sfPref %>%
  dplyr::filter(prefCode >= 26 & prefCode <= 30) %>%
  sf::st_union()

#Dataframe Filtered
dfPrefPop <- dfPref %>%
  dplyr::left_join(dfPop, by = "prefCode") %>%
  dplyr::rename(N = pop)

#+++++++++++
# Ratio of Daytime and Nighttime Population
#+++++++++++

#OD Flows across Prefectures
sfFlow <- sf::read_sf("data/shp/SHP_lines_od_flows_by_pref_time14.shp")
sfFlow <- sfFlow %>%
  dplyr::rename(prefCodeOrigin = prefCdO) %>%
  dplyr::rename(prefCodeDestination = prefCdD) %>%
  dplyr::rename(prefNameOrigin = prefNmO) %>%
  dplyr::rename(prefNameDestination = prefNmD) %>%
  dplyr::rename(periodOfDay = prdOfDy) %>%
  dplyr::rename(periodOfTIme = prdOfTm) %>%
  dplyr::rename(totalPopOrigin = ttlPpOr)

#OD Flow Matrix
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
}

#Merge COVID-19 Data
sfPrefCovid <- sfPref %>%
  dplyr::left_join(dfPrefPop, by = "prefCode")

#Calculate Daytime Population
N_daytime_weekday <- matrix(t(listC[[4]][[1]]) %*% sfPrefCovid$N, 47, 1)
N_daytime_weekend <- matrix(t(listC[[4]][[2]]) %*% sfPrefCovid$N, 47, 1)
dfN <- tibble(prefCode = dfPref$prefCode, 
              N_daytime_weekday = N_daytime_weekday,
              N_daytime_weekend = N_daytime_weekend)

#Add Variable
sfPrefCovidNratio <- sfPrefCovid %>%
  dplyr::left_join(dfN, by = "prefCode") %>%
  dplyr::mutate(ratio_n_weekday = N_daytime_weekday / N) %>%
  dplyr::mutate(ratio_n_weekend = N_daytime_weekend / N)

#Map Visualization
p4 <- ggplot(sfPrefCovidNratio) +
  geom_sf(aes(fill = ratio_n_weekday), lwd = 0.15, color = "black") +
  scale_fill_gradient2(low = "midnightblue", 
                       high = "darkred", 
                       midpoint = 1,
                       limits=c(0.85, 1.25),
                       name = "") +
  geom_sf(data = sfPrefTokyo, lwd = 0.4, alpha = 0, color="black", show.legend = FALSE) +
  geom_sf(data = sfPrefNagoya, lwd = 0.4, alpha = 0, color="black", show.legend = FALSE) +
  geom_sf(data = sfPrefOsaka, lwd = 0.4, alpha = 0, color="black", show.legend = FALSE) +
  geom_label_repel(aes(label = "Greater\nTokyo\nArea", geometry = geometry), data = sfPrefTokyo, stat = "sf_coordinates", nudge_x = 2, nudge_y = -250, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nNagoya\nArea", geometry = geometry), data = sfPrefNagoya, stat = "sf_coordinates", nudge_x = -2, nudge_y = 300, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nOsaka\nArea", geometry = geometry), data = sfPrefOsaka, stat = "sf_coordinates", nudge_x = -2, nudge_y = -200, size = 2.8) +
  ggtitle("(a) Weekday in April 2016") +
  theme_void() +
  theme(
    plot.title  = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.text=element_text(size = 8),
    legend.title=element_text(size = 8),
    legend.margin=margin(l = -1.3, b = -1.5, unit='cm')
  )
p4

#Map Visualization
p5 <- ggplot(sfPrefCovidNratio) +
  geom_sf(aes(fill = ratio_n_weekend), lwd = 0.15, color = "black") +
  geom_sf(data = sfPrefTokyo, lwd = 0.4, alpha = 0, color="black", show.legend = FALSE) +
  geom_sf(data = sfPrefNagoya, lwd = 0.4, alpha = 0, color="black", show.legend = FALSE) +
  geom_sf(data = sfPrefOsaka, lwd = 0.4, alpha = 0, color="black", show.legend = FALSE) +
  geom_label_repel(aes(label = "Greater\nTokyo\nArea", geometry = geometry), data = sfPrefTokyo, stat = "sf_coordinates", nudge_x = 2, nudge_y = -250, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nNagoya\nArea", geometry = geometry), data = sfPrefNagoya, stat = "sf_coordinates", nudge_x = -2, nudge_y = 300, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nOsaka\nArea", geometry = geometry), data = sfPrefOsaka, stat = "sf_coordinates", nudge_x = -2, nudge_y = -200, size = 2.8) +
  scale_fill_gradient2(low = "midnightblue", 
                       high = "darkred", 
                       midpoint = 1,
                       limits=c(0.85, 1.25),
                       name = "") +
  ggtitle("(b) Weekend and Holiday in April 2016") +
  theme_void() +
  theme(
    plot.title  = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.text=element_text(size = 8),
    legend.title=element_text(size = 8),
    legend.margin=margin(l = -1.3, b = -1.5, unit='cm')
  )
p5

#Patchwork
g2 <- p4 + p5 +
  plot_annotation(
    caption = "Ratio of daytime and nighttime population",
    theme = theme(
      plot.caption = element_text(size = 10)
    )
  )
g2

#Save
ggsave(file = "output/fig/paper/fig_map_ratio_population_time14.eps", plot = g2)
ggsave(file = "output/fig/paper/fig_map_ratio_population_time14.svg", plot = g2)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekday_time14.eps", plot = p4)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekday_time14.svg", plot = p4)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekend_time14.eps", plot = p5)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekend_time14.svg", plot = p5)
