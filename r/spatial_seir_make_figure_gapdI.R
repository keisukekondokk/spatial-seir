#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
foreach(i = 0:47, .packages = c("scales", "ggplot2", "dplyr", "ggrepel")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp1 <- dfResultsLong1 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(dI = if_else(date < startDay, NA_real_, dI))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(dI = if_else(date < startDay, NA_real_, dI))
  #Dataframe at Starting Date of Simulation
  dfTemp3 <- dfTemp1 %>% 
    filter(date == startDay+1)
  #Dataframe for Gap
  dfTemp4 <- dfTemp1 %>% 
    dplyr::left_join(dfTemp2 %>% select(date, prefCode, dI), by = c("date", "prefCode")) %>%
    dplyr::rename(dI_with_mobility = dI.x) %>%
    dplyr::rename(dI_without_mobility = dI.y) %>%
    dplyr::mutate(gapdI = dI_without_mobility - dI_with_mobility)
  #Dataframe at Starting Date of Simulation
  dfTemp5 <- dfTemp4 %>% 
    filter(date == startDay+1)
  
  #Filename
  saveFileNamePng <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_GapdI_Pref", 
      sprintf("%02d", i), 
      ".png"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_GapdI_Pref", 
      sprintf("%02d", i), 
      ".eps"
    )
  
  #Line Color
  colorLine <- c("#f45b5b")
  
  #ggplot2
  ggplot() +
    geom_line(aes(x = date, y = gapdI, color = "Effects of interregional mobility restriction on the number of new positive cases"),
              size = 1.3, 
              linetype = "solid",
              data = dfTemp4) +
    geom_point(aes(x = date-1, y = gapdI), data = dfTemp5, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date-1, y = gapdI, label = as.character(date-1)), data = dfTemp5, vjust = 2, hjust = 1, size = 3) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(limits=c(round(min(dfTemp4$gapdI),2), round(max(dfTemp4$gapdI),2)), labels = comma_format(accuracy = 1)) +
    scale_x_date(date_breaks = "3 months",
                 date_labels = "%Y\n%m-%d") + 
    ylab("Gap in New Positive Cases") +
    xlab("") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          legend.text=element_text(size = 7),
          legend.margin=margin(l = 0, b = -1.0, unit='cm'),
          axis.text.x = element_text(size = 7),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNamePng, dpi = 200, width = 6, height = 3)
  ggsave(file = saveFileNameEps, dpi = 200, width = 6, height = 3)
}
parallel::stopCluster(cl)
