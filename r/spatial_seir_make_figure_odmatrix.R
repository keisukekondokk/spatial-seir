#+++++++++++++++
# Load Data
#+++++++++++++++
dfPref <- readr::read_csv("data/pref/CSV_list_pref_en.csv")
numPref <- nrow(dfPref)

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


#+++++++++++++++
# Tile for 47 Prefectures
#+++++++++++++++

p1 <- list()
#
for(i in 1:2){
  dfMatrix <- dfFlow %>%
    filter(year == 2016) %>%
    filter(month == 4) %>%
    filter(periodOfDay == i) 
  
  #Tile Plot
  p1[[i]] <- ggplot(dfMatrix, aes(x = prefCodeDestination, y = prefCodeOrigin)) +
    geom_tile(aes(fill = shFlow)) +
    coord_fixed() +
    scale_fill_gradientn(name = "Share", colours = c("snow1", "seagreen4", "darkslategrey")) +
    scale_x_continuous(breaks=c(1,10,20,30,40,47), position = "top") +
    scale_y_reverse(breaks=c(1,10,20,30,40,47)) + 
    ylab(label = "Origin Prefecture") +
    xlab(label = "Destination Prefecture") +
    theme_classic() +
    theme(legend.text=element_text(size = 8),
          legend.title=element_text(size = 8),
          legend.position = "bottom",
          legend.margin=margin(l = 0.5, t = -0.2, unit='cm'),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          panel.background = element_rect(size=1, color = "black", linetype="solid"),
          panel.grid.minor = element_blank())
  
  #Save Each Plot
  filename <- paste0("output/fig/paper/fig_tile_national_", i, "_mobility.eps")
  ggsave(file = filename, plot = p1[[i]])
  
  #Add Title
  if(i == 1){
    p1[[i]] <- p1[[i]] + 
      ggtitle("Weekday") +
      theme(
        plot.title = element_text(size = 10, hjust = 0.5)
      )
    guides(fill=FALSE)
  }
  if(i == 2){
    p1[[i]] <- p1[[i]] + 
      ggtitle("Weekend and Holiday") +
      theme(
        plot.title = element_text(size = 10, hjust = 0.5)
      )
    guides(fill=FALSE)
  }
  
}

#Patchwork
g1 <- p1[[1]] + p1[[2]] + 
  plot_annotation(
    title  = "(a) 47 Prefectures",
    theme = theme(
      plot.title  = element_text(size = 12, hjust = 0.5, face = "bold")
    )
  )
g1

#Save
filename <- paste0("output/fig/paper/fig_tile_national_mobility.eps")
filenamePng <- paste0("output/fig/paper/fig_tile_national_mobility.png")
ggsave(file = filename, plot = g1)
ggsave(file = filenamePng, plot = g1)

#+++++++++++++++
# Tile for Greater Tokyo Area
#+++++++++++++++

p2 <- list()
#
for(i in 1:2){
  dfMatrix <- dfFlow %>%
    filter(year == 2016) %>%
    filter(month == 4) %>%
    filter(periodOfDay == i) %>%
    filter(prefCodeOrigin >= 11 & prefCodeOrigin <= 14) %>%
    filter(prefCodeDestination >= 11 & prefCodeDestination <= 14)
  
  #
  p2[[i]] <- ggplot(dfMatrix, aes(x = prefCodeDestination, y = prefCodeOrigin)) +
    geom_tile(aes(fill = shFlow)) +
    coord_fixed() +
    scale_fill_gradientn(name = "Share", colours = c("snow1", "seagreen4", "darkslategrey")) +
    scale_x_continuous(breaks=seq(1,47,1), position = "top") +
    scale_y_reverse(breaks=seq(1,47,1)) + 
    ylab(label = "Origin Prefecture") +
    xlab(label = "Destination Prefecture") +
    theme_classic() +
    theme(legend.text=element_text(size = 12),
          legend.title=element_text(size = 12),
          legend.position = "bottom",
          legend.margin=margin(l = 0.5, t = -0.2, unit='cm'),
          axis.title.y =element_text(size = 14),
          axis.title.x =element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          panel.background = element_rect(size=1, color = "black", linetype="solid"),
          panel.grid.minor = element_blank())
  
  #Save Each Plot
  filename <- paste0("output/fig/paper/fig_tile_tokyo_", i, "_mobility.eps")
  ggsave(file = filename, plot = p2[[i]])
  
  #Add Title
  if(i == 1){
    p2[[i]] <- p2[[i]] + 
      ggtitle("Weekday") +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    guides(fill=FALSE)
  }
  if(i == 2){
    p2[[i]] <- p2[[i]] + 
      ggtitle("Weekend and Holiday") +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    guides(fill=FALSE)
  }
  
}

#Patchwork
g2 <- p2[[1]] + p2[[2]] + 
  plot_annotation(
    title = "(b) Greater Tokyo Area",
    theme = theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold")
    )
  )
g2

#Save
filename <- paste0("output/fig/paper/fig_tile_tokyo_mobility.eps")
filenamePng <- paste0("output/fig/paper/fig_tile_tokyo_mobility.png")
ggsave(file = filename, plot = g2)
ggsave(file = filenamePng, plot = g2)

#+++++++++++++++
# Tile for Greater Osaka Area
#+++++++++++++++
p3 <- list()
#
for(i in 1:2){
  dfMatrix <- dfFlow %>%
    filter(year == 2016) %>%
    filter(month == 4) %>%
    filter(periodOfDay == i) %>%
    filter(prefCodeOrigin >= 25 & prefCodeOrigin <= 30) %>%
    filter(prefCodeDestination >= 25 & prefCodeDestination <= 30)
  
  #Tile Plot
  p3[[i]] <- ggplot(dfMatrix, aes(x = prefCodeDestination, y = prefCodeOrigin)) +
    geom_tile(aes(fill = shFlow)) +
    coord_fixed() +
    scale_fill_gradientn(name = "Share", colours = c("snow1", "seagreen4", "darkslategrey")) +
    scale_x_continuous(breaks=seq(1,47,1), position = "top") +
    scale_y_reverse(breaks=seq(1,47,1)) + 
    ylab(label = "Origin Prefecture") +
    xlab(label = "Destination Prefecture") +
    theme_classic() +
    theme(legend.text=element_text(size = 12),
          legend.title=element_text(size = 12),
          legend.position = "bottom",
          legend.margin=margin(l = 0.5, t = -0.2, unit='cm'),
          axis.title.y =element_text(size = 14),
          axis.title.x =element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          panel.background = element_rect(size=1, color = "black", linetype="solid"),
          panel.grid.minor = element_blank())
  
  #Save Each Plot
  filename <- paste0("output/fig/paper/fig_tile_osaka_", i, "_mobility.eps")
  ggsave(file = filename, plot = p3[[i]])
  
  #Add Title
  if(i == 1){
    p3[[i]] <- p3[[i]] + 
      ggtitle("Weekday") +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    guides(fill=FALSE)
  }
  if(i == 2){
    p3[[i]] <- p3[[i]] + 
      ggtitle("Weekend and Holiday") +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    guides(fill=FALSE)
  }
  
}

#Patchwork
g3 <- p3[[1]] + p3[[2]] + 
  plot_annotation(
    title = "(c) Greater Osaka Area",
    theme = theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold")
    )
  )
g3

#Save
filename <- paste0("output/fig/paper/fig_tile_osaka_mobility.eps")
filenamePng <- paste0("output/fig/paper/fig_tile_osaka_mobility.png")
ggsave(file = filename, plot = g3)
ggsave(file = filenamePng, plot = g3)