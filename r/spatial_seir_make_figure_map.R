#+++++++++++++++
# Load Data
#+++++++++++++++


#Prefecture Code and Name
dfPref <- readr::read_csv("data/pref/CSV_list_pref_en.csv")
numPref <- nrow(dfPref)

#Dataframe
dfPanelDB <- readr::read_csv("output/csv/case04/CSV_covid19_panel_by_pref.csv")

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



#+++++++++++++++
# Ratio of Daytime and Nighttime Population
#+++++++++++++++

#Dataframe Filtered
dfPrefCovid1 <- dfPanelDB %>%
  dplyr::filter(date == as.Date("2020-08-17", "%Y-%m-%d")) %>%
  dplyr::filter(prefCode != 0)
#
dfPrefCovid2 <- dfPanelDB %>%
  dplyr::filter(date == as.Date("2020-11-10", "%Y-%m-%d")) %>%
  dplyr::filter(prefCode != 0)

#Shapefile
sfPrefCovid1 <- sfPref %>%
  dplyr::left_join(dfPrefCovid1, by = "prefCode")
#Shapefile
sfPrefCovid2 <- sfPref %>%
  dplyr::left_join(dfPrefCovid2, by = "prefCode")

#Breaks in Figure
equal1 <- c(0, 1, 10, 20, 50, 100, 150, max(sfPrefCovid1$newPositive)) 
equal2 <- c(0, 1, 10, 20, 50, 100, 150, 200, 250, max(sfPrefCovid2$newPositive)) 

#Custom labels
labels_equal1 <- imap_chr(equal1, function(., idx){
  return(paste0("[", round(equal1[idx], 0), ", ", round(equal1[idx + 1], 0), ")"))
})
labels_equal2 <- imap_chr(equal2, function(., idx){
  return(paste0("[", round(equal2[idx], 0), ", ", round(equal2[idx + 1], 0), ")"))
})

#Drop labels not used
labels_equal1 <- labels_equal1[1:7]
labels_equal2 <- labels_equal2[1:length(labels_equal2) - 1]

#Add New Variables for Figure
sfPrefCovid1 <- sfPrefCovid1 %>%
  mutate(newPositive_equal = cut(newPositive, breaks = equal1, labels = labels_equal1, right = FALSE, include.lowest = TRUE,))
#
sfPrefCovid2 <- sfPrefCovid2 %>%
  mutate(newPositive_equal = cut(newPositive, breaks = equal2, labels = labels_equal2, right = FALSE, include.lowest = TRUE))

#Use the same color
colorPal9 <- brewer.pal(9, "YlOrRd")
colorPal9[1] <- "#FFFFFF"
colorPal6 <- colorPal9[1:6]

#Map visualization
p1 <- ggplot(sfPrefCovid1) +
  geom_sf(aes(fill = newPositive_equal), lwd = 0.15, color = "black") +
  geom_sf(data = sfPrefTokyo, lwd = 0.4, color="black", alpha = 0, show.legend = FALSE) +
  geom_sf(data = sfPrefNagoya, lwd = 0.4, color="black", alpha = 0, show.legend = FALSE) +
  geom_sf(data = sfPrefOsaka, lwd = 0.4, color="black", alpha = 0, show.legend = FALSE) +
  geom_label_repel(aes(label = "Greater\nTokyo\nArea", geometry = geometry), data = sfPrefTokyo, stat = "sf_coordinates", vjust = 2.0, hjust = 0.2, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nNagoya\nArea", geometry = geometry), data = sfPrefNagoya, stat = "sf_coordinates", vjust = -1.8, hjust = 0.6, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nOsaka\nArea", geometry = geometry), data = sfPrefOsaka, stat = "sf_coordinates", vjust = 1.6, hjust = 0.6, size = 2.8) +
  scale_fill_manual(values  = colorPal6,
                    name = "",
                    guide = guide_legend(
                      keyheight = unit(5, units = "mm"),
                      title.position = "top",
                      reverse = T # display highest income on top
                    )) +
  ggtitle(paste0("(a) ", sfPrefCovid1$date)) +
  theme_void() +
  theme(
    plot.title  = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.text=element_text(size = 8),
    legend.title=element_text(size = 8),
    legend.margin=margin(l = -2.1, b = -1.7, unit='cm')
  )
p1


#Map visualization
p2 <- ggplot(sfPrefCovid2) +
  geom_sf(aes(fill = newPositive_equal), lwd = 0.15, color = "black") +
  geom_sf(data = sfPrefTokyo, lwd = 0.4, color="black", alpha = 0, show.legend = FALSE) +
  geom_sf(data = sfPrefNagoya, lwd = 0.4, color="black", alpha = 0, show.legend = FALSE) +
  geom_sf(data = sfPrefOsaka, lwd = 0.4, color="black", alpha = 0, show.legend = FALSE) +
  geom_label_repel(aes(label = "Greater\nTokyo\nArea", geometry = geometry), data = sfPrefTokyo, stat = "sf_coordinates", vjust = 2.0, hjust = 0.2, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nNagoya\nArea", geometry = geometry), data = sfPrefNagoya, stat = "sf_coordinates", vjust = -1.8, hjust = 0.6, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nOsaka\nArea", geometry = geometry), data = sfPrefOsaka, stat = "sf_coordinates", vjust = 1.6, hjust = 0.6, size = 2.8) +
  scale_fill_manual(values  = colorPal9,
                    name = "",
                    guide = guide_legend(
                      keyheight = unit(5, units = "mm"),
                      title.position = "top",
                      reverse = T # display highest income on top
                    )) +
  ggtitle(paste0("(b) ", sfPrefCovid2$date)) +
  theme_void() +
  theme(
    plot.title  = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.text=element_text(size = 8),
    legend.title=element_text(size = 8),
    legend.margin=margin(l = -2.1, b = -1.7, unit='cm')
  )
p2


#Patchwork
g1 <- p1 + p2+
  plot_annotation(
    caption = "Daily number of new positive cases",
    theme = theme(
      plot.caption = element_text(size = 10)
    )
  )
g1

#Save
ggsave(file = "output/fig/paper/fig_map_covid19.eps", plot = g1)
ggsave(file = "output/fig/paper/fig_map_covid19.png", plot = g1)



#+++++++++++
# Ratio of Daytime and Nighttime Population
#+++++++++++

#OD Flows across Prefectures
sfFlow <- sf::read_sf("data/shp/SHP_lines_od_flows_by_pref.shp")
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
  dplyr::left_join(dfPrefCovid1, by = "prefCode")

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
  geom_label_repel(aes(label = "Greater\nTokyo\nArea", geometry = geometry), data = sfPrefTokyo, stat = "sf_coordinates", vjust = 2.0, hjust = 0.2, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nNagoya\nArea", geometry = geometry), data = sfPrefNagoya, stat = "sf_coordinates", vjust = -1.8, hjust = 0.6, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nOsaka\nArea", geometry = geometry), data = sfPrefOsaka, stat = "sf_coordinates", vjust = 1.6, hjust = 0.6, size = 2.8) +
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
  geom_label_repel(aes(label = "Greater\nTokyo\nArea", geometry = geometry), data = sfPrefTokyo, stat = "sf_coordinates", vjust = 2.0, hjust = 0.2, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nNagoya\nArea", geometry = geometry), data = sfPrefNagoya, stat = "sf_coordinates", vjust = -1.8, hjust = 0.6, size = 2.8) +
  geom_label_repel(aes(label = "Greater\nOsaka\nArea", geometry = geometry), data = sfPrefOsaka, stat = "sf_coordinates", vjust = 1.6, hjust = 0.6, size = 2.8) +
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
ggsave(file = "output/fig/paper/fig_map_ratio_population.eps", plot = g2)
ggsave(file = "output/fig/paper/fig_map_ratio_population.png", plot = g2)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekday.eps", plot = p4)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekday.png", plot = p4)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekend.eps", plot = p5)
ggsave(file = "output/fig/paper/fig_map_ratio_population_weekend.png", plot = p5)
