#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
foreach(i = 0:47, .packages = c("scales", "ggplot2", "dplyr", "ggrepel")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp0 <- dfPanelDB %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(!is.na(S)) %>%
    dplyr::filter(date <= endDayFigure)
  #Dataframe of List 1
  dfTemp1 <- dfResultsLong1 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(S = if_else(date < startDay, NA_real_, S))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(S = if_else(date < startDay, NA_real_, S))
  #Dataframe at Starting Date of Simulation
  dfTemp3 <- dfTemp0 %>% 
    filter(date == startDay)
  
  #Filename
  saveFileNamePng <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_S_Pref", 
      sprintf("%02d", i), 
      ".png"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_S_Pref", 
      sprintf("%02d", i), 
      ".eps"
    )
  
  #Label for Each Case Scenario
  if(numCaseScenario == 6){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model \nwith interregional mobility except Tokyo"
    labelLine2 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    colorLine <- c("#2f7ed8", "#20b2aa", "#f45b5b")
  } else{
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    labelLine2 <- "Simulated numbers from SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#f45b5b", "#25b086")
  }
  
  #ggplot2
  ggplot() +
    geom_line(aes(x = date, y = S, color = labelLine0),
              size = 1.1, 
              linetype = "solid",
              data = dfTemp0) +
    geom_line(aes(x = date, y = S, color = labelLine2),
              size = 1.3, 
              linetype = "solid",
              data = dfTemp2) +
    geom_line(aes(x = date, y = S, color = labelLine1), 
              size = 1.2, 
              linetype = "solid",
              data = dfTemp1) +
    geom_point(aes(x = date, y = S), data = dfTemp3, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date, y = S, label = as.character(date)), data = dfTemp3, vjust = -3, hjust = 1, size = 3) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(labels = comma_format(accuracy = 1)) +
    scale_x_date(date_breaks = "3 months",
                 date_labels = "%Y\n%m-%d") + 
    ylab("Number of Susceptible") +
    xlab("") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          legend.text=element_text(size = 7),
          legend.margin=margin(l = -1.5, b = -1.0, unit='cm'),
          axis.text.x = element_text(size = 7),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNamePng, dpi = 200, width = 6, height = 3)
  ggsave(file = saveFileNameEps, dpi = 200, width = 6, height = 3)
}
parallel::stopCluster(cl)
