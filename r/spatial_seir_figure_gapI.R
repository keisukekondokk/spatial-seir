#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
foreach(i = 0:numPref, .packages = c("scales", "ggplot2", "dplyr", "ggrepel")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp1 <- dfResultsLong1 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date >= startDayFigure) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(I = if_else(date < startDay, NA_real_, I))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date >= startDayFigure) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(I = if_else(date < startDay, NA_real_, I))
  #Dataframe at Starting Date of Simulation
  dfTemp3 <- dfTemp1 %>% 
    filter(date == startDay+1)
  #Dataframe for Gap
  if(numCaseScenario <= 2){
    dfTemp4 <- dfTemp1 %>% 
    dplyr::left_join(dfTemp2 %>% select(date, prefCode, I), by = c("date", "prefCode")) %>%
    dplyr::rename(I_data1 = I.x) %>%
    dplyr::rename(I_data2 = I.y) %>%
    dplyr::mutate(gapI = I_data2 - I_data1)
  }
  if(numCaseScenario >= 3){
      dfTemp4 <- dfTemp1 %>% 
        dplyr::left_join(dfTemp2 %>% select(date, prefCode, I), by = c("date", "prefCode")) %>%
        dplyr::rename(I_data1 = I.x) %>%
        dplyr::rename(I_data2 = I.y) %>%
        dplyr::mutate(gapI = I_data1 - I_data2)
  }
  #Dataframe at Starting Date of Simulation
  dfTemp5 <- dfTemp4 %>% 
    filter(date == startDay+1)
  
  #Filename
  saveFileNameSvg <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_GapI_Pref", 
      sprintf("%02d", i), 
      ".svg"
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
  
  #X Axis
  if(numCaseScenario == 1){
    x_date_breaks <- "1 year"
    x_date_limits <- c(startDayFigure, endDayFigure)
  } else {
    x_date_breaks <- "1 year"
    x_date_limits <- c(startDayFigure, endDayFigure)
  }
  
  #Line Color
  colorLine <- c("#f45b5b")
  
  #Label Location
  nudge_y_ggrepel <- 0
  
  #ggplot2
  ggplot() +
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_line(aes(x = date, y = gapI),
              size = 1.3, 
              linetype = "solid",
              data = dfTemp4) +
    geom_point(aes(x = date-1, y = gapI), data = dfTemp5, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date-1, y = gapI, label = as.character(date-1)), data = dfTemp5, nudge_x = -150, nudge_y = nudge_y_ggrepel, size = 3) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(limits=c(round(min(dfTemp4$gapI),2), round(max(dfTemp4$gapI),2)), labels = comma_format(accuracy = 1)) +
    scale_x_date(date_breaks = x_date_breaks,
                 limits = c(startDayFigure, endDayFigure),
                 date_labels = "%Y\n%m-%d") + 
    ylab("Gap in Number of Infectious Individuals") +
    xlab("") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          legend.text=element_text(size = 7),
          legend.margin=margin(l = 0, b = -1.0, unit='cm'),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 8),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNameSvg, dpi = 200, width = 6, height = 3)
  ggsave(file = saveFileNameEps, dpi = 200, width = 6, height = 3)
}
parallel::stopCluster(cl)
