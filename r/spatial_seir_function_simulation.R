#fSimulation(vX[1:2])
#vX[1]: integer. 1: simulation with interregional mobility 2: simulation without interregional mobility
#vX[2]: integer. number of case scenario (1L to 6L)

#Simulation from Starting Date to End Date
fSimulation <- function(vX) {
  #LOOP START------------------------------------------------
  #Unit of Iteration: Day
  for(t in 1:time) {
    
    #Inputs of function
    k <- vX[1]
    idxCase <- vX[2]
    
    #OD Matrix for List 1
    if(k == 1){
      if(idxCase == 6){
        #Mobility Restriction only for Tokyo
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t]+1
        mC <- listC[[idxListC1]][[idxListC2]]
        #Stay in residential prefectures instead of staying in Tokyo.
        for(m in 1:47) {
          mC[m, m] <- mC[m, m] + mC[m, 13]
          mC[m, 13] <- 0
        }
        #Tokyo Lockdown
        mC[13,] <- 0
        mC[13, 13] <- 1
      } else{
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t]+1
        mC <- listC[[idxListC1]][[idxListC2]]
      }
    }
    #OD Matrix for List 2
    if(k == 2){
      mC <- mIdentity
      if(idxCase == 6){
        #Free Mobility
        idxListC1 <- dfSnapShotTime$date_month[t] 
        idxListC2 <- dfSnapShotTime$date_holiday[t]+1
        mC <- listC[[idxListC1]][[idxListC2]]
      } else{
        #Mobility Restriction (Diagonal Matrix)
        mC <- mIdentity
      }
    }
    
    #NPIs Degree for Case Scenario
    dfTemp <- listAlpha[[idxCase]] %>%
      dplyr::filter(year == dfSnapShotTime$date_year[t]) %>%
      dplyr::filter(month == dfSnapShotTime$date_month[t]) 
    alpha <- as.double(dfTemp$alpha)
    beta <- alpha * beta0
    
    #Number of Susceptible in the daytime (element by element)
    numer1 <- mC * matrix(mS[t,], region, region, byrow = FALSE)
    #Number of Infectious in the daytime
    numer2 <- matrix(t(mC)%*%mI[t,], region, region, byrow = TRUE)
    #Number of Total Population in the daytime
    denom <- matrix(t(mC)%*%vN, region, region, byrow = TRUE)
    #Number of Infectious in the daytime (element by element)
    numer <- numer1 * numer2
    temp <- matrix(rowSums(numer/denom), region, 1, byrow = FALSE)
    
    #Transition
    vdS <- - beta * temp
    vdE <- -vdS - epsilon*mE[t,]
    vdI <- epsilon*mE[t,] - gamma*mI[t,]
    vdR <- gamma*mI[t,]
    vRatio <- matrix((t(mC) %*% mI[t, ] / t(mC) %*% vN) / (t(mIdentity) %*% mI[t, ] / t(mIdentity) %*% vN), region, 1, byrow = FALSE)
    
    #Store Results
    mS[t+1,] <- mS[t,] + vdS
    mE[t+1,] <- mE[t,] + vdE
    mI[t+1,] <- mI[t,] + vdI
    mdI[t,] <- epsilon*mE[t,]
    mR[t+1,] <- mR[t,] + vdR
    mRatio[t,] <- vRatio
    mBeta[t,] <- beta
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
    dplyr::bind_cols(as_tibble(mBeta)) %>%
    dplyr::bind_cols(as_tibble(mRatio)) %>%
    dplyr::filter(rownumberPost > 1) %>%
    dplyr::left_join(dfSnapShotTime, by = "rownumberPost") %>%
    dplyr::select(-rownumberPost) %>%
    dplyr::filter(!is.na(date))
  
  #
  dfLongAdd <- dfSimulationResultsAdd %>%
    tidyr::pivot_longer(-c(numDays, date, date_year, date_month, date_day, date_holiday),
                        names_to = c(".value", "prefCode"), 
                        names_pattern = "(Beta|Ratio)(.*)") %>%
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
    dplyr::left_join(dfLongAdd %>% select(date, prefCode, Beta, Ratio), by = c("date" = "date", "prefCode" = "prefCode")) %>%
    dplyr::mutate(Beta = if_else(prefCode == 0 & date >= startDay, NA_real_, Beta)) %>%
    dplyr::mutate(Ratio = if_else(prefCode == 0 & date >= startDay, 1, Ratio)) %>%
    dplyr::filter(!is.na(date))
  
  #Return Results as Datafame
  return(dfResultsLong)
}
