#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
listPlot <- list()
foreach(i = 0:numPref, .packages = c("scales", "ggplot2", "dplyr", "ggrepel", "stringr")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp1 <- dfResultsLong1 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date >= startDayFigure) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(RatioLambda = if_else(date < startDay, NA_real_, RatioLambda))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date >= startDayFigure) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(RatioLambda = if_else(date < startDay, NA_real_, RatioLambda))
  #Dataframe of List 3
  dfTemp3 <- dfResultsLong3 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date >= startDayFigure) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(RatioLambda = if_else(date < startDay, NA_real_, RatioLambda))
  #Dataframe at Starting Date of Simulation
  dfTemp9 <- dfTemp1 %>% 
    filter(date == startDay + 1)

  #Filename
  saveFileNameSvg <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_RatioLambda_Pref", 
      sprintf("%02d", i), 
      ".svg"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_RatioLambda_Pref", 
      sprintf("%02d", i), 
      ".eps"
    )
  
  #Label for Each Case Scenario
  if(numCaseScenario == 1){
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model \nwith interregional mobility"
    labelLine2 <- "Simulated numbers from SEIR model\nwithout interregional mobility"
    colorLine <- c("#f45b5b", "#25b086")
  }
  if(numCaseScenario == 2){
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model \nwith interregional mobility"
    labelLine2 <- "Simulated numbers from SEIR model\nwithout interregional mobility"
    colorLine <- c("#f45b5b", "#25b086")
  }
  if(numCaseScenario == 3){
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers \nfrom SEIR model \nwith interregional mobility at 8pm"
    labelLine2 <- "Simulated numbers \nfrom SEIR model\nwith interregional mobility at 2pm"
    labelLine3 <- "Simulated numbers \nfrom SEIR model\nwithout interregional mobility"
    colorLine <- c("#483d8b", "#f45b5b", "#25b086")
  }
  if(numCaseScenario == 4){
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers \nfrom SEIR model \nwith interregional mobility \nexcept infectious persons"
    labelLine2 <- "Simulated numbers \nfrom SEIR model\nwith interregional mobility"
    labelLine3 <- "Simulated numbers \nfrom SEIR model\nwithout interregional mobility"
    colorLine <- c("#483d8b", "#f45b5b", "#25b086")
  }
  if(numCaseScenario == 5){
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers \nfrom SEIR model \nwith interregional mobility \nexcept Greater Tokyo Area"
    labelLine2 <- "Simulated numbers \nfrom SEIR model\nwithout interregional mobility"
    labelLine3 <- "Simulated numbers \nfrom SEIR model\nwithout interregional mobility"
    colorLine <- c("#483d8b", "#f45b5b", "#25b086")
  }
  if(numCaseScenario == 6){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers \nfrom SEIR model \nwith interregional mobility \nexcept Greater Osaka Area"
    labelLine2 <- "Simulated numbers \nfrom SEIR model\nwith interregional mobility"
    labelLine3 <- "Simulated numbers \nfrom SEIR model\nwithout interregional mobility"
    colorLine <- c("#483d8b", "#f45b5b", "#25b086")
  }
  if(numCaseScenario == 7){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers \nfrom SEIR model \nwith interregional mobility \nexcept Tokyo and Osaka"
    labelLine2 <- "Simulated numbers \nfrom SEIR model\nwith interregional mobility"
    labelLine3 <- "Simulated numbers \nfrom SEIR model\nwithout interregional mobility"
    colorLine <- c("#483d8b", "#f45b5b", "#25b086")
  }
  
  #Label Location
  nudge_y_ggrepel <- - 0.05 * max(dfTemp1$RatioLambda, na.rm = TRUE)
  
  #X Axis
  if(numCaseScenario == 1){
    x_date_breaks <- "1 year"
    x_date_limits <- c(startDayFigure, endDayFigure)
  } else {
    x_date_breaks <- "1 year"
    x_date_limits <- c(startDayFigure, endDayFigure)
  }
  
  #ggplot2
  if(numCaseScenario == 1 | numCaseScenario == 2 ){
    ggtemp <- ggplot() +
      geom_line(aes(x = date, y = RatioLambda, color = labelLine2),
                size = 1.3, 
                linetype = "solid",
                data = dfTemp2) +
      geom_line(aes(x = date, y = RatioLambda, color = labelLine1), 
                size = 1.2, 
                linetype = "solid",
                data = dfTemp1)
  } else{
    ggtemp <- ggplot() +
      geom_line(aes(x = date, y = RatioLambda, color = labelLine3),
                size = 1.3, 
                linetype = "solid",
                data = dfTemp3) +
      geom_line(aes(x = date, y = RatioLambda, color = labelLine2),
                size = 1.3, 
                linetype = "solid",
                data = dfTemp2) +
      geom_line(aes(x = date, y = RatioLambda, color = labelLine1), 
                size = 1.2, 
                linetype = "solid",
                data = dfTemp1)
  }
  listPlot[[i+1]] <- ggtemp +
    geom_point(aes(x = date - 1, y = RatioLambda), data = dfTemp9, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date - 1, y = RatioLambda, label = as.character(date-1)), data = dfTemp9, nudge_x = -150, nudge_y = nudge_y_ggrepel, size = 3) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(limits=c(round(min(dfTemp1$RatioLambda), 3), round(max(dfTemp1$RatioLambda), 3)), labels = comma_format(accuracy = .001)) +
    scale_x_date(date_breaks = x_date_breaks,
                 limits = c(startDayFigure, endDayFigure),
                 date_labels = "%Y\n%m-%d") + 
    ylab("Ratio of daytime and nighttime\n force of infection") +
    xlab("") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          legend.text=element_text(size = 7),
          legend.margin=margin(l = -1.5, b = -1.0, unit='cm'),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 4, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNameSvg, plot = listPlot[[i+1]], dpi = 200, width = 6, height = 3)
  ggsave(file = saveFileNameEps, plot = listPlot[[i+1]], dpi = 200, width = 6, height = 3)
}
parallel::stopCluster(cl)
