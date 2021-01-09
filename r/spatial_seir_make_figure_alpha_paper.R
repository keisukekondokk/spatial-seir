#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
foreach(i = 0:47, .packages = c("scales", "ggplot2", "dplyr", "ggrepel")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp1 <- dfResultsLong1 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(Alpha = if_else(date < startDay, NA_real_, Beta/beta0))
  #Dataframe of List 2
  dfTemp2 <- dfResultsLong2 %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(date <= endDayFigure) %>%
    dplyr::mutate(Alpha = if_else(date < startDay, NA_real_, Beta/beta0))
  #Dataframe at Starting Date of Simulation
  dfTemp3 <- dfTemp1 %>% 
    filter(date == startDay+1)

  #Filename
  saveFileNamePng <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_Alpha_Pref", 
      sprintf("%02d", i), 
      "_Paper.png"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_Alpha_Pref", 
      sprintf("%02d", i), 
      "_Paper.eps"
    )
  
  #Label for Each Case Scenario
  if(numCaseScenario == 6){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model \nwith interregional mobility except Tokyo"
    labelLine2 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  }
  if(numCaseScenario == 7){
    #
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model \nwith interregional mobility except Greater Tokyo area"
    labelLine2 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  } else{
    labelLine0 <- "Observed numbers"
    labelLine1 <- "Simulated numbers from SEIR model\nwith interregional mobility"
    labelLine2 <- "Simulated numbers from SEIR model\nwithout interregional mobility"
    colorLine <- c("#2f7ed8", "#f45b5b", "#25b086")
  }
  
  #Title
  if(numCaseScenario == 1){
    labelTitle <- "Rapid Convergence Scenario"
  }
  if(numCaseScenario == 2){
    labelTitle <- "Modest Convergence Scenario"
  }
  if(numCaseScenario == 3){
    labelTitle <- "Modest Convergence Scenario"
  }
  if(numCaseScenario == 4){
    labelTitle <- "Modest Convergence Scenario"
  }
  if(numCaseScenario == 5){
    labelTitle <- "Worsening Scenario"
  }
  if(numCaseScenario == 6){
    labelTitle <- "Modest Convergence Scenario"
  }
  if(numCaseScenario == 7){
    labelTitle <- "Worsening Scenario"
  }
  
  #
  date_break_manual <- c(seq(from = as.Date("2020-01-01"), to = as.Date("2024-01-01"), by = "6 months"))
  
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
    geom_label_repel(aes(x = date-1, y = Alpha, label = as.character(date-1)), data = dfTemp3, vjust = -3, hjust = 1, size = 6) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(breaks=seq(0, 1, by = 0.1), limits=c(0, 1), labels = comma_format(accuracy = .1)) +
    scale_x_date(breaks = date_break_manual,
                 date_labels = "%Y\n%b") + 
    ylab("Degree of NPIs\n(Scaling Factor for Transmission Rate)") +
    xlab("") +
    ggtitle(labelTitle) +
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          legend.title=element_blank(),
          legend.position = "none",
          legend.text=element_text(size = 14),
          legend.margin=margin(l = -1.5, b = -1.0, unit='cm'),
          axis.title.y =element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNamePng, dpi = 200, width = 6, height = 4)
  ggsave(file = saveFileNameEps, dpi = 200, width = 6, height = 4)
  #
  Sys.setlocale("LC_ALL", localeOriginal)
}
parallel::stopCluster(cl)
