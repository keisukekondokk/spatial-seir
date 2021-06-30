#GGPLOT2
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
listPlot <- list()
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
  dfTemp9 <- dfTemp1 %>% 
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
      "/FIG_Alpha_Pref", 
      sprintf("%02d", i), 
      ".svg"
    )
  #Filename
  saveFileNameEps <-
    paste0(
      "output/fig/case",
      sprintf("%02d", numCaseScenario),
      "/FIG_Alpha_Pref", 
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
  
  #Label Location
  nudge_y_ggrepel <- 0.2 * max(dfTemp2$Alpha, na.rm = TRUE)
  
  #ggplot2
  listPlot[[i+1]] <- ggplot() +
    geom_hline(yintercept = 1/R0, linetype="dashed") +
    geom_line(aes(x = date, y = Alpha),
              size = 1.3, 
              linetype = "solid",
              data = dfTemp2) +
    geom_point(aes(x = date-1, y = Alpha), data = dfTemp9, size = 2, shape = 22, fill="transparent", stroke = 2, color = "red") +
    geom_label_repel(aes(x = date-1, y = Alpha, label = labelDate), data = dfTemp9, nudge_x = -200, nudge_y = nudge_y_ggrepel, size = 4) +
    scale_y_continuous(breaks=seq(0, 1, by = 0.1), limits=c(0, 1), labels = comma_format(accuracy = .1)) +
    scale_x_date(date_breaks = x_date_breaks,
                 limits = c(startDayFigure, endDayFigure),
                 date_labels = "%Y\n%m-%d") + 
    ylab("Scaling Factor for Transmission Rate") +
    xlab("") +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "top",
          legend.text=element_text(size = 7),
          legend.margin=margin(l = -1.5, b = -1.0, unit='cm'),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(title.position = "top", ncol = 3, nrow = 2, byrow = TRUE))
  
  #Save
  ggsave(file = saveFileNameSvg, plot = listPlot[[i+1]], dpi = 200, width = 6, height = 3)
  ggsave(file = saveFileNameEps, plot = listPlot[[i+1]], dpi = 200, width = 6, height = 3)
}
parallel::stopCluster(cl)
