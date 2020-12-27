#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
listPlot <- list()
foreach(i = 0:47, .packages = c("scales", "ggplot2", "dplyr", "ggrepel", "stringr")) %dopar% {
  print(paste0("Prefecture: ", i))
  #Dataframe of List 1
  dfTemp0 <- dfPanelDB %>%
    dplyr::filter(prefCode == i) %>%
    dplyr::filter(!is.na(I)) %>%
    dplyr::filter(date <= endDayFigure)
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
  dfTemp3 <- dfTemp0 %>% 
    filter(date == startDay) %>%
    mutate(labelDate = paste(
      str_sub(as.character(date), start = "1", end = "4"),
      str_sub(as.character(date), start = "6", end = "10"),
      sep = "\n"
    ))
  
  #Filename
  saveFileNamePng <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_I_Pref", 
      sprintf("%02d", i), 
      "_Paper.png"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_I_Pref", 
      sprintf("%02d", i), 
      "_Paper.eps"
    )
  
  #Label for Each Case Scenario
  if(numCaseScenario == 6){
    #
    labelLine0 <- "Observed\nnumbers"
    labelLine1 <- "SEIR model with \ninterregional mobility\nexcept Tokyo" #space is necessary after with
    labelLine2 <- "SEIR model with\ninterregional mobility"
    colorLine <- c("#2f7ed8", "#483d8b", "#f45b5b")
  } else{
    labelLine0 <- "Observed\nnumbers"
    labelLine1 <- "SEIR model with\ninterregional mobility"
    labelLine2 <- "SEIR model without\ninterregional mobility"
    colorLine <- c("#2f7ed8", "#f45b5b", "#25b086")
  }
  
  #Legend Display Option
  #optionLegend <- "top"
  optionLegend <- "none"

  #Title
  if(i == 0){
    labelTitle <- paste0(sprintf("%02d ", i), "National Total")
  } else{
    labelTitle <- paste0(sprintf("%02d ", i), dfPref$prefNameEn[i])
  }
    
  #ggplot2
  localeOriginal <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_ALL","English")
  listPlot[[i+1]] <- ggplot() +
    geom_line(aes(x = date, y = I, color = labelLine0),
              size = 1.5, 
              linetype = "solid",
              data = dfTemp0) +
    geom_line(aes(x = date, y = I, color = labelLine2),
              size = 1.6, 
              linetype = "solid",
              data = dfTemp2) +
    geom_line(aes(x = date, y = I, color = labelLine1), 
              size = 1.5, 
              linetype = "solid",
              data = dfTemp1) +
    geom_point(aes(x = date, y = I), data = dfTemp3, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date, y = I, label = labelDate), data = dfTemp3, vjust = -3, hjust = 1, size = 6) +
    scale_color_manual(values = colorLine) + 
    scale_y_continuous(labels = comma_format(accuracy = 1)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y\n%b") + 
    ylab("Number of Infectious") +
    xlab("") +
    ggtitle(labelTitle) +
    theme_bw() +
    theme(plot.title = element_text(size = 28, face = "bold"),
          legend.title=element_blank(),
          legend.position = optionLegend,
          legend.text=element_text(size = 14),
          legend.margin=margin(l = -1.5, b = -1.0, unit='cm'),
          axis.title.y =element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNamePng, plot = listPlot[[i+1]], dpi = 200, width = 6, height = 4)
  ggsave(file = saveFileNameEps, plot = listPlot[[i+1]], dpi = 200, width = 6, height = 4)
  #
  Sys.setlocale("LC_ALL", localeOriginal)
}
parallel::stopCluster(cl)