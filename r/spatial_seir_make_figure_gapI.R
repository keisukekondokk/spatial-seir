#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
foreach(i = 0:47, .packages = c("scales", "ggplot2", "dplyr", "ggrepel")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp1 <- dfResultsLong1 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(I = if_else(date < startDay, NA_real_, I))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(I = if_else(date < startDay, NA_real_, I))
  #Dataframe at Starting Date of Simulation
  dfTemp3 <- dfTemp1 %>% 
    filter(date == startDay+1)
  #Dataframe for Gap
  dfTemp4 <- dfTemp1 %>% 
    dplyr::left_join(dfTemp2 %>% select(date, prefCode, I), by = c("date", "prefCode")) %>%
    dplyr::mutate(gapI = I.x - I.y)
  #Dataframe at Starting Date of Simulation
  dfTemp5 <- dfTemp4 %>% 
    filter(date == startDay+1)
  
  #Filename
  saveFileNamePng <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_GapI_Pref", 
      sprintf("%02d", i), 
      ".png"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_GapI_Pref", 
      sprintf("%02d", i), 
      ".eps"
    )
  
  #Label for Each Case Scenario
  if(numCaseScenario == 6){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model \nwith interregional mobility except Tokyo"
    labelLine2 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    colorLine <- c("#483d8b", "#f45b5b")
  } else{
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    labelLine2 <- "Simulated numbers from SEIR model\nwithout interregional mobility"
    colorLine <- c("#f45b5b", "#25b086")
  }
  
  #ggplot2
  ggplot() +
    geom_line(aes(x = date, y = gapI, color = "Gap in number of infectious individuals between SEIR models with and without interregional mobility"),
              size = 1.3, 
              linetype = "solid",
              data = dfTemp4) +
    geom_point(aes(x = date-1, y = gapI), data = dfTemp5, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date-1, y = gapI, label = as.character(date-1)), data = dfTemp5, vjust = -3, hjust = 1, size = 3) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(limits=c(round(min(dfTemp4$gapI),2), round(max(dfTemp4$gapI),2)), labels = comma_format(accuracy = 1)) +
    scale_x_date(date_breaks = "3 months",
                 date_labels = "%Y\n%m-%d") + 
    ylab("Gap in number of infectious individuals") +
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
