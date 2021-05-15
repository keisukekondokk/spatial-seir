#fSimulation(vX[1:2])
#vX[1]: integer. 1: simulation with interregional mobility 2: simulation without interregional mobility
#vX[2]: integer. number of case scenario (1L to 7L)

#Simulation from Starting Date to End Date
fSimulation <- function(vX) {
  #LOOP START------------------------------------------------
  #Unit of Iteration: Day
  for(t in 1:time) {
    
    #Inputs of function
    k <- vX[1]
    idxCase <- vX[2]
    
    #=======================
    #OD Matrix for List 1
    #=======================
    if(k == 1){
      if(idxCase == 1){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 2){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 3){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[2]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 4){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mIdentity
        mCR <- mC
      }
      if(idxCase == 5){
        #Mobility Restriction only for Greater Tokyo Area
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        #Stay in residential prefectures instead of staying in Greater Tokyo Area
        for(m in 1:47) {
          if(m < 11 | m > 14){
            mC[m, m] <- mC[m, m] + mC[m, 11] + mC[m, 12] + mC[m, 13] + mC[m, 14]
            mC[m, 11:14] <- 0
          }
          if(m == 11 | m == 12 | m == 13 | m == 14){
            mC[m,] <- 0
            mC[m, m] <- 1
          }
        }
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 6){
        #Mobility Restriction only for Greater Osaka Area
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        #Stay in residential prefectures instead of staying in Greater Osaka Area
        for(m in 1:47) {
          if(m < 26 | m > 28){
            mC[m, m] <- mC[m, m] + mC[m, 26] + mC[m, 27] + mC[m, 28]
            mC[m, 26:28] <- 0
          }
          if(m == 26 | m == 27 | m == 28){
            mC[m,] <- 0
            mC[m, m] <- 1
          }
        }
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 7){
        #Mobility Restriction only for Tokyo and Osaka
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        #Stay in residential prefectures instead of staying in Tokyo and Osaka
        for(m in 1:47) {
          if(m == 13 | m == 27){
            mC[m,] <- 0
            mC[m, m] <- 1
          }
        }
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
    }
    #=======================
    #OD Matrix for List 2
    #=======================
    if(k == 2){
      if(idxCase == 1){
        #Mobility Restriction (Diagonal Matrix)
        mC <- mIdentity 
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 2){
        #Mobility Restriction (Diagonal Matrix)
        mC <- mIdentity 
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 3){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
        
      }
      if(idxCase == 4){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 5){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 6){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
      if(idxCase == 7){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t] + 1
        mC <- listC[[1]][[idxListC1]][[idxListC2]]
        mCS <- mC
        mCE <- mC
        mCI <- mC
        mCR <- mC
      }
    }
    
    #NPIs Degree for Case Scenario
    if(flagSmoothingAlpha == TRUE){
      iNumDays <- dfCalendarAlpha$numDays[t]
      dfTempAlpha <- dfCalendarAlpha %>%
        dplyr::filter(numDays == iNumDays) 
      vAlpha <- as.numeric(dfTempAlpha$alphaMa)
    } else {
      iNumDays <- dfCalendarAlpha$numDays[t]
      dfTempAlpha <- dfCalendarAlpha %>%
        dplyr::filter(numDays == iNumDays) 
      vAlpha <- as.numeric(dfTempAlpha$alpha)
    }
    vBeta <- vAlpha * beta0
    
    #Number of Susceptible in the daytime (element by element)
    numer1 <- mCS * matrix(mS[t,], region, region, byrow = FALSE)
    #Number of Infectious in the daytime
    numer2 <- matrix(t(mCI)%*%mI[t,], region, region, byrow = TRUE)
    #Number of Total Population in the daytime
    denom <- 
      matrix(t(mCS)%*%mS[t,], region, region, byrow = TRUE) +
      matrix(t(mCE)%*%mE[t,], region, region, byrow = TRUE) +
      matrix(t(mCI)%*%mI[t,], region, region, byrow = TRUE) +
      matrix(t(mCR)%*%mR[t,], region, region, byrow = TRUE)

    #Number of Infectious in the daytime (element by element)
    numer <- numer1 * numer2
    temp <- matrix(rowSums(numer/denom), region, 1, byrow = FALSE)
    
    #Transition
    vdS <- - vBeta * temp
    vdE <- - vdS - epsilon*mE[t,]
    vdI <- epsilon*mE[t,] - gamma*mI[t,]
    vdR <- gamma*mI[t,]
    
    #Ratio of Force of Infection
    vRatioLambda <- matrix((t(mCI) %*% mI[t, ] / (t(mCS) %*% mS[t, ] + t(mCE) %*% mE[t, ] + t(mCI) %*% mI[t, ] + t(mCR) %*% mR[t, ])) / 
                             (t(mIdentity) %*% mI[t, ] / t(mIdentity) %*% vN), region, 1, byrow = FALSE)
    
    #Infinite
    vRatioLambda[is.infinite(vRatioLambda)] <- NA_real_
    
    #Store Results
    mS[t+1,] <- mS[t,] + vdS
    mE[t+1,] <- mE[t,] + vdE
    mI[t+1,] <- mI[t,] + vdI
    mdI[t,] <- epsilon*mE[t,]
    mR[t+1,] <- mR[t,] + vdR
    mRatioLambda[t,] <- vRatioLambda
    mAlpha[t,] <- vAlpha
  }  
  #LOOP END------------------------------------------------
  
  #DataFrame
  dfTime <- tibble::tibble(rownumberPost = rep(1:(time+1))) 
  
  #DataFrame From Matrix
  dfSimulationResultsLevel <- dfTime %>%
    dplyr::bind_cols(as_tibble(mN)) %>%
    dplyr::bind_cols(as_tibble(mS)) %>%
    dplyr::bind_cols(as_tibble(mE)) %>%
    dplyr::bind_cols(as_tibble(mI)) %>%
    dplyr::bind_cols(as_tibble(mdI)) %>%
    dplyr::bind_cols(as_tibble(mR)) %>%
    dplyr::filter(rownumberPost > 1)
  
  #Merge Calender Info
  dfSimulationResultsLevel <- dfSimulationResultsLevel %>%
    dplyr::left_join(dfSnapShotTime, by = "rownumberPost") %>%
    dplyr::select(-rownumberPost)
  
  #Append Pre- and Post-Simuration Dataframe
  dfSimulationResultsLevelFull <- dplyr::bind_rows(dfPreWide, dfSimulationResultsLevel) %>%
    dplyr::mutate(numDays = row_number())
  
  #Make DataFrame of Long Panel
  dfLong <- dfSimulationResultsLevelFull %>%
    tidyr::pivot_longer(-c(numDays, date, date_year, date_month, date_day, date_holiday),
                        names_to = c(".value", "prefCode"), 
                        names_pattern = "(N|S|E|I|dI|R|)(.*)") %>%
    dplyr::arrange(prefCode, date)
  
  #Dataframe for National Total
  dfResultsLongNation <- dfLong %>%
    dplyr::filter(prefCode != 0) %>%
    dplyr::group_by(numDays, date, date_year, date_month, date_day, date_holiday) %>%
    dplyr::summarise(N = sum(N), 
                     S = sum(S),
                     E = sum(E),
                     I = sum(I),
                     dI = sum(dI),
                     R = sum(R)) %>%
    dplyr::mutate(prefCode = 0) %>%
    dplyr::ungroup()
  
  #Force of infection
  dfSimulationResultsAdd <- dfTime %>%
    dplyr::bind_cols(as_tibble(mAlpha)) %>%
    dplyr::bind_cols(as_tibble(mRatioLambda)) %>%
    dplyr::filter(rownumberPost > 1) %>%
    dplyr::left_join(dfSnapShotTime, by = "rownumberPost") %>%
    dplyr::select(-rownumberPost) %>%
    dplyr::filter(!is.na(date))
  
  #
  dfLongAdd <- dfSimulationResultsAdd %>%
    tidyr::pivot_longer(-c(numDays, date, date_year, date_month, date_day, date_holiday),
                        names_to = c(".value", "prefCode"), 
                        names_pattern = "(Alpha|RatioLambda)(.*)") %>%
    dplyr::mutate(prefCode = as.numeric(prefCode)) %>%
    dplyr::arrange(prefCode, date)
  
  #Append National Total
  dfResultsLong <- dfLong %>%
    dplyr::filter(prefCode != 0) %>%
    dplyr::mutate(prefCode = as.numeric(prefCode)) %>%
    dplyr::bind_rows(dfResultsLongNation) %>%
    dplyr::select(date, prefCode, N, S, E, I, dI, R) %>%
    dplyr::arrange(prefCode, date)
  #%>%
  #  dplyr::mutate(shS = S/N) %>%
  #  dplyr::mutate(shE = E/N) %>%
  #  dplyr::mutate(shI = I/N) %>%
  #  dplyr::mutate(shR = R/N)
  
  #Merge Dataframe 
  dfResultsLong <- dfResultsLong %>%
    dplyr::left_join(dfLongAdd %>% select(date, prefCode, Alpha, RatioLambda), by = c("date" = "date", "prefCode" = "prefCode")) %>%
    dplyr::mutate(Alpha = if_else(prefCode == 0 & date >= startDay, NA_real_, Alpha)) %>%
    dplyr::mutate(RatioLambda = if_else(prefCode == 0 & date >= startDay, 1, RatioLambda)) %>%
    dplyr::filter(!is.na(date))
  
  #Return Results as Datafame
  return(dfResultsLong)
}
