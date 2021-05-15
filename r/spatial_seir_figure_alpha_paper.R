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
    dplyr::mutate(Alpha = if_else(date < startDay, NA_real_, Alpha))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date >= startDayFigure) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(Alpha = if_else(date < startDay, NA_real_, Alpha))
  #Dataframe at Starting Date of Simulation
  dfTemp3 <- dfTemp1 %>% 
    filter(date == startDay+1) %>%
    mutate(labelDate = paste(
      stringr::str_sub(as.character(date-1), start = "1", end = "4"),
      stringr::str_sub(as.character(date-1), start = "6", end = "10"),
      sep = "\n"
    ))

  #Filename
  saveFileNameSvg <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/paper/FIG_Alpha_Pref", 
      sprintf("%02d", i), 
      "_Paper.svg"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/paper/FIG_Alpha_Pref", 
      sprintf("%02d", i), 
      "_Paper.eps"
    )
  
  #Label for Each Case Scenario
  if(numCaseScenario == 1){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model \nwith interregional mobility"
    labelLine2 <- "SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#f45b5b", "#25b086")
  }
  if(numCaseScenario == 2){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model \nwith interregional mobility"
    labelLine2 <- "SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#f45b5b", "#25b086")
  }
  if(numCaseScenario == 3){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model\nwith interregional mobility"
    labelLine2 <- "SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  }
  if(numCaseScenario == 4){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model \nwith interregional mobility except infectious persons"
    labelLine2 <- "SEIR model\nwith interregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  }
  if(numCaseScenario == 5){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model \nwith interregional mobility except Greater Tokyo Area"
    labelLine2 <- "SEIR model\nwith interregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  }
  if(numCaseScenario == 6){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model \nwith interregional mobility except Greater Osaka Area"
    labelLine2 <- "SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  }
  if(numCaseScenario == 7){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "SEIR model \nwith interregional mobility except Greater Tokyo area"
    labelLine2 <- "SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#f45b5b", "#25b086")
  } 
  
  #Legend Display Option
  #optionLegend <- "top"
  optionLegend <- "none"
  
  #X Axis
  if(numCaseScenario == 1){
    x_date_breaks <- "1 year"
    x_date_limits <- c(startDayFigure, endDayFigure)
  } else {
    x_date_breaks <- "1 year"
    x_date_limits <- c(startDayFigure, endDayFigure)
  }

  #Y Axis
  if(max(dfTemp2$Alpha, na.rm = TRUE) <= 1.0){
    y_break_manual <- seq(0, 1.0, by = 0.2)
    y_limits <- c(0, 1.0)
  }
  if(max(dfTemp2$Alpha, na.rm = TRUE) > 1.0){
    y_break_manual <- seq(0, max(dfTemp2$Alpha, na.rm = TRUE), by = 0.2)
    y_limits <- c(0, max(dfTemp2$Alpha, na.rm = TRUE))
  }
  if(max(dfTemp2$Alpha, na.rm = TRUE) > 2.5){
    y_break_manual <- seq(0, max(dfTemp2$Alpha, na.rm = TRUE), by = 0.5)
    y_limits <- c(0, max(dfTemp2$Alpha, na.rm = TRUE))
  }
  if(max(dfTemp2$Alpha, na.rm = TRUE) > 10){
    y_break_manual <- seq(0, max(dfTemp2$Alpha, na.rm = TRUE), by = 2)
    y_limits <- c(0, max(dfTemp2$Alpha, na.rm = TRUE))
  }
  if(max(dfTemp2$Alpha, na.rm = TRUE) > 20){
    y_break_manual <- seq(0, max(dfTemp2$Alpha, na.rm = TRUE), by = 5)
    y_limits <- c(0, max(dfTemp2$Alpha, na.rm = TRUE))
  }
  
  #Label Location
  nudge_y_ggrepel <- 0.8 * max(dfTemp2$Alpha, na.rm = TRUE)
  if(numCaseScenario == 1){
    nudge_x_ggrepel <- 100
  } else {
    nudge_x_ggrepel <- -150
  }
  
  #Title
  if(i == 0){
    labelTitle <- paste0(sprintf("%02d ", i), "National Total")
  } else{
    labelTitle <- paste0(sprintf("%02d ", i), dfPref$prefNameEn[i])
  }
  
  #ggplot2
  localeOriginal <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_ALL","English")
  ggplot() +
    geom_hline(yintercept = 1/R0, linetype="dashed") +
    geom_line(aes(x = date, y = Alpha),
              size = 1.3, 
              linetype = "solid",
              data = dfTemp2) +
    geom_point(aes(x = date-1, y = Alpha), data = dfTemp3, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date-1, y = Alpha, label = labelDate), data = dfTemp3, nudge_x = nudge_x_ggrepel, nudge_y = nudge_y_ggrepel, size = 8) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(breaks=y_break_manual, limits=y_limits, labels = comma_format(accuracy = .1)) +
    scale_x_date(breaks = x_date_breaks,
                 limits = x_date_limits,
                 date_labels = "%Y\n%b") + 
    ylab("Scaling Factor for \nTransmission Rate") +
    xlab("") +
    ggtitle(labelTitle) +
    theme_bw() +
    theme(plot.title = element_text(size = 28, face = "bold"),
          legend.title=element_blank(),
          legend.position = optionLegend,
          legend.text=element_text(size = 14),
          legend.margin=margin(l = -1.5, b = -1.0, unit='cm'),
          axis.title.y =element_text(size = 24),
          axis.text.y = element_text(size = 24),
          axis.text.x = element_text(size = 24),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNameSvg, dpi = 200, width = 6, height = 4)
  ggsave(file = saveFileNameEps, dpi = 200, width = 6, height = 4)
  #
  Sys.setlocale("LC_ALL", localeOriginal)
}
parallel::stopCluster(cl)
