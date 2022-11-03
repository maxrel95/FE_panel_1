library(data.table) 
library(readxl)
library( plm )
library( dplyr )
library(fixest)
library(lmtest)
library(sandwich)
library(estimatr)


summaryOLS = function( model, data, var, name ){
  # this function 
  resume = summary( model )
  res = resume$coefficients[2, c("Estimate", "t value")]
  res['Explanatory power'] = res["Estimate"]*( sd( data %>% select(var[1]) %>% unlist(),
                                                   na.rm = TRUE ) / sd( data %>% select(var[2]) %>% unlist(),
                                                   na.rm = TRUE ) )
  res = as.data.frame( res )
  colnames(res) = name
  return(res)
}

summaryfeols = function( model, data, var, name ){
  resume = summary( model )
  x = summary(model)
  interest = data.frame(c(x$coefficients[1], x$coefficients[1]/x$se[1],
                          x$coefficients[1]*(sd( data %>% select(var[1]) %>% unlist(),
                          na.rm = TRUE ) / sd( data %>% select(var[2]) %>% unlist(),
                          na.rm = TRUE ))),
                        row.names = c('Estimate', 't value', "Explanatory power"))
  colnames(interest) = name
  return(interest)
}

# this is the function that run all the analysis
banksAsLiquidityProvider = function( dateStart, dateEnd){
  # create a string variable that takes the year of studies to adjust the name of the results that are saved
  dateToUse = paste(substr(  dateStart, 3,4), substr(  dateEnd, 3,4), sep = "")

    # import the data 
  pnl <- as.data.frame(fread( "PS1_data/callreports_1976_2021wc.csv" )) # read the data

    # indicate to the machine that the vector date is actually date datatype
  pnl$date = strptime(pnl$date, format = "%Y%m%d")
  
  # slice the data and keep only the period under study and the variables of interest
  df9296 = pnl[pnl$date>=dateStart & pnl$date<=dateEnd, c("date", "rssdid", "bhcid", "chartertype",
                                                                  "fedfundsrepoasset", "securities","assets",
                                                                  "cash", "transdep", "deposits", "commitments",
                                                                  "loans","ciloans", "persloans", "reloans")]
  
  # put some zero where the bhcid is missing
  if (sum(is.na(df9296$bhcid))>0){
    df9296$bhcid[is.na(df9296$bhcid)] = 0
  }
  
  # replace the bhcids which are equal to zero by their rssid as they are not part of a holding
  df9296$bhcid[df9296$bhcid==0] = df9296$rssdid[df9296$bhcid==0]
  
  # find the all possible chartertype
  chartertype = unique(df9296$chartertype)
  
  filteredData = df9296%>%
    na.omit() %>% # remove row with missing values
    filter(chartertype==200 | chartertype==300 | chartertype==320 | chartertype==340) %>% # keeps only banks that are of these charter types
    group_by(bhcid, date ) %>% # group the bank by bhcid and by date
    summarise(fedfundsrepoasset=sum(fedfundsrepoasset), securities=sum(securities), assets=sum(assets),
              cash=sum(cash), transdep=sum(transdep),  deposits=sum(deposits),
              commitments=sum(commitments),  loans=sum(loans),
              ciloans=sum(ciloans), persloans=sum(persloans) , reloans=sum(reloans))%>% # aggregate the variables at the holding level by summing 
    ungroup()%>% # remove the group
    group_by(bhcid) %>% # group by holding company bank
    filter(n()>=8) %>% # keep only bhcid with more than 8 observation
    ungroup() %>% # ungroup
    mutate( bhcid = as.factor( bhcid),
            date = as.factor( as.character( date ) ) ) # define date and bhcid as factor to create dummies for FE
  
  nbrOfFirms = length(unique(filteredData$bhcid)) # compute the number of bank holding company
  print(nbrOfFirms)
  
  dt = data.table() # create an empty table store the data needed for the regressions from raw data
  dt[, bhcid:=filteredData$bhcid] # create a vector with bank holding id in dt
  dt[, date:=filteredData$date] # add the date
  dt[, SECRAT:=(filteredData$fedfundsrepoasset + filteredData$securities)/filteredData$assets] # compute the secrat variables
  dt[, LIQRAT:=SECRAT + filteredData$cash/filteredData$assets] # compute the LIQRAT
  dt[, DEPRAT:=filteredData$transdep / filteredData$deposits] # same as above  for DEPRAT
  dt[, COMRAT:=filteredData$commitments/(filteredData$commitments+filteredData$loans)] # same for COMRAT
  dt[, ASSET:=log(filteredData$assets)] # add the log of the asset
  dt[, ciloans:=filteredData$ciloans/filteredData$loans] # relative ciloans 
  dt[, persloans:=filteredData$persloans/filteredData$loans] # relative persloans 
  dt[, reloans:=filteredData$reloans/filteredData$loans] # relative reloans 
  
  crossSectionalTimeAveraged = dt %>%
    na.omit() %>% # if any missing values remove them 
    as.data.frame() %>% # format the table as data.frame type 
    group_by(bhcid) %>% # group data by bhcid 
    summarise(LIQRAT=mean(LIQRAT, na.rm=TRUE), SECRAT=mean(SECRAT, na.rm=TRUE), DEPRAT=mean(DEPRAT, na.rm=TRUE),
              COMRAT=mean(COMRAT, na.rm=TRUE), ASSET=mean(ASSET, na.rm=TRUE),ciloans=mean(ciloans, na.rm=TRUE),
              persloans=mean(persloans, na.rm=TRUE), reloans=mean(reloans, na.rm=TRUE)) %>% # compute the time mean for each bank for all variables
    ungroup() # ungroup the data 
  
  cstaBig = crossSectionalTimeAveraged %>%
    arrange(desc(ASSET)) %>% # sort data by assets
    slice(1:100) # keep only the 100 largest
  cstaMid = crossSectionalTimeAveraged %>%
    arrange(desc(ASSET)) %>% # sort data by assets
    slice(101:600) # keep only the 101 to 600 largest
  cstaSmall = crossSectionalTimeAveraged %>% # sort data by assets
    arrange(desc(ASSET)) %>% # sort data by assets
    slice(601:nbrOfFirms) # keep from the 601th to the last
  
  q = c(0.25, 0.5, 0.75) # define the differnt quartile to compute 
  
  # create the table that concatenate quartile for all size category
  t1 = cbind(rbind(quantile(crossSectionalTimeAveraged$LIQRAT, probs = q, na.rm = TRUE), # compute quartiles for all size for each variables
                   quantile(crossSectionalTimeAveraged$SECRAT, probs = q, na.rm = TRUE),
                   quantile(crossSectionalTimeAveraged$DEPRAT, probs = q, na.rm = TRUE),
                   quantile(crossSectionalTimeAveraged$COMRAT, probs = q, na.rm = TRUE)),
             rbind(quantile(cstaBig$LIQRAT, probs = q, na.rm = TRUE), # compute quartiles for the 100 largest 
                   quantile(cstaBig$SECRAT, probs = q, na.rm = TRUE),
                   quantile(cstaBig$DEPRAT, probs = q, na.rm = TRUE),
                   quantile(cstaBig$COMRAT, probs = q, na.rm = TRUE)),
             rbind(quantile(cstaMid$LIQRAT, probs = q, na.rm = TRUE), # compte quartile for the 101 to 600 largest 
                   quantile(cstaMid$SECRAT, probs = q, na.rm = TRUE),
                   quantile(cstaMid$DEPRAT, probs = q, na.rm = TRUE),
                   quantile(cstaMid$COMRAT, probs = q, na.rm = TRUE)),
             rbind(quantile(cstaSmall$LIQRAT, probs = q, na.rm = TRUE), # compte quartile for the 601 to last
                   quantile(cstaSmall$SECRAT, probs = q, na.rm = TRUE),
                   quantile(cstaSmall$DEPRAT, probs = q, na.rm = TRUE),
                   quantile(cstaSmall$COMRAT, probs = q, na.rm = TRUE)))
  rownames(t1) = c("LIQRAT", "SECRAT", "DEPRAT", "COMRAT") # rename rows
  tabI = round(as.data.frame(t1), digits = 3) # round and define as data frame
  
  rm(df9296, t1) # remove variables 
  
  ######### Q2 #########
  ######## Table III #########
  m1 = lm_robust(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = crossSectionalTimeAveraged, se_type = 'HC1') # run regression according to formulaon time averaged data for full sample 
  res1 = summaryOLS(m1, crossSectionalTimeAveraged, c("DEPRAT", "LIQRAT"), "All Banks") # get beta coeff, tstat and explanatory power  
  
  m1 = lm_robust(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaBig, se_type = 'HC1') # regression but for big bank
  res2 = summaryOLS(m1, cstaBig, c("DEPRAT", "LIQRAT"), "Large Banks") # get beta coeff, tstat and explanatory power
  
  m1 = lm_robust(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaMid, se_type = 'HC1') # regression with medium banks
  res3 = summaryOLS(m1, cstaMid, c("DEPRAT", "LIQRAT"), "Medium Banks")
  
  m1 = lm_robust(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaSmall, se_type = 'HC1') # regression with small banks
  res4 = summaryOLS(m1, cstaSmall, c("DEPRAT", "LIQRAT"), "Small Banks")
  
  tabIIIa = cbind( res1, res2, res3, res4) # combine result across the size
  
  # same as above from line 132 to 146 but the dependant variable is SECRAT instead of LIQRAT
  m1 = lm_robust(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = crossSectionalTimeAveraged, se_type = 'HC1')
  res1 = summaryOLS(m1, crossSectionalTimeAveraged, c("DEPRAT", "SECRAT"), "All Banks")
  
  m1 = lm_robust(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaBig, se_type = 'HC1')
  res2 = summaryOLS(m1, cstaBig, c("DEPRAT", "SECRAT"), "Large Banks")
  
  m1 = lm_robust(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaMid, se_type = 'HC1')
  res3 = summaryOLS(m1, cstaMid, c("DEPRAT", "SECRAT"), "Medium Banks")
  
  m1 = lm_robust(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaSmall, se_type = 'HC1')
  res4 = summaryOLS(m1, cstaSmall, c("DEPRAT", "SECRAT"), "Small Banks")
  
  tabIIIb = cbind( res1, res2, res3, res4)
  
  tabIII = rbind( tabIIIa, tabIIIb) # concatenate result to reproduce table III
  
  # same as line 132 to 165 but remove the control variables
  m1 = lm_robust(LIQRAT ~ DEPRAT,
          data = crossSectionalTimeAveraged, se_type = 'HC1')
  res1 = summaryOLS(m1, crossSectionalTimeAveraged, c("DEPRAT", "LIQRAT"), "All Banks")
  
  m1 = lm_robust(LIQRAT ~ DEPRAT,
          data = cstaBig, se_type = 'HC1')
  res2 = summaryOLS(m1, cstaBig, c("DEPRAT", "LIQRAT"), "Large Banks")
  
  m1 = lm_robust(LIQRAT ~ DEPRAT,
          data = cstaMid, se_type = 'HC1')
  res3 = summaryOLS(m1, cstaMid, c("DEPRAT", "LIQRAT"), "Medium Banks")
  
  m1 = lm_robust(LIQRAT ~ DEPRAT,
          data = cstaSmall, se_type = 'HC1')
  res4 = summaryOLS(m1, cstaSmall, c("DEPRAT", "LIQRAT"), "Small Banks")
  
  tabIIIa = cbind( res1, res2, res3, res4)
  
  
  m1 = lm_robust(SECRAT ~ DEPRAT,
          data = crossSectionalTimeAveraged, se_type = 'HC1')
  res1 = summaryOLS(m1, crossSectionalTimeAveraged, c("DEPRAT", "SECRAT"), "All Banks")
  
  m1 = lm_robust(SECRAT ~ DEPRAT,
          data = cstaBig, se_type = 'HC1')
  res2 = summaryOLS(m1, cstaBig, c("DEPRAT", "SECRAT"), "Large Banks")
  
  m1 = lm_robust(SECRAT ~ DEPRAT,
          data = cstaMid, se_type = 'HC1')
  res3 = summaryOLS(m1, cstaMid, c("DEPRAT", "SECRAT"), "Medium Banks")
  
  m1 = lm_robust(SECRAT ~ DEPRAT,
          data = cstaSmall, se_type = 'HC1')
  res4 = summaryOLS(m1, cstaSmall, c("DEPRAT", "SECRAT"), "Small Banks")
  
  tabIIIb = cbind( res1, res2, res3, res4)
  
  tabIIINoControl = rbind( tabIIIa, tabIIIb)
  
  ##### Tab IV #######
  # same as for line 132 to 210 but with COMRAT as dependent variable to reproduce the table IV with and without control variables
  m1 = lm_robust(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = crossSectionalTimeAveraged, se_type = 'HC1')
  res1 = summaryOLS(m1, crossSectionalTimeAveraged, c("DEPRAT", "COMRAT"), "All Banks")
  
  m1 = lm_robust(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaBig, se_type = 'HC1')
  res2 = summaryOLS(m1, cstaBig, c("DEPRAT", "COMRAT"), "Large Banks")
  
  m1 = lm_robust(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaMid, se_type = 'HC1')
  res3 = summaryOLS(m1, cstaMid, c("DEPRAT", "COMRAT"), "Medium Banks")
  
  m1 = lm_robust(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
          data = cstaSmall, se_type = 'HC1')
  res4 = summaryOLS(m1, cstaSmall, c("DEPRAT", "COMRAT"), "Small Banks")
  
  tabIV = cbind( res1, res2, res3, res4)
  
  # remove controle variables
  m1 = lm_robust(COMRAT ~ DEPRAT,
          data = crossSectionalTimeAveraged, se_type = 'HC1')
  res1 = summaryOLS(m1, crossSectionalTimeAveraged, c("DEPRAT", "COMRAT"), "All Banks")
  
  m1 = lm_robust(COMRAT ~ DEPRAT,
          data = cstaBig, se_type = 'HC1')
  res2 = summaryOLS(m1, cstaBig, c("DEPRAT", "COMRAT"), "Large Banks")
  
  m1 = lm_robust(COMRAT ~ DEPRAT,
          data = cstaMid, se_type = 'HC1')
  res3 = summaryOLS(m1, cstaMid, c("DEPRAT", "COMRAT"), "Medium Banks")
  
  m1 = lm_robust(COMRAT ~ DEPRAT,
          data = cstaSmall, se_type = 'HC1')
  res4 = summaryOLS(m1, cstaSmall, c("DEPRAT", "COMRAT"), "Small Banks")
  
  tabIVNoControl = cbind( res1, res2, res3, res4)
  
  rm( m1, res1, res2, res3, res4, tabIIIa, tabIIIb)
  
  ######### Q3 ##########
  # use binsreg to visualize the relationship, provide the control variables to take them into account in the plot
  binsreg(y = LIQRAT, x= DEPRAT
          , w = ~ASSET + ciloans + persloans + reloans,
          data = as.data.frame( crossSectionalTimeAveraged),
          nbins = 50, # use 50 point to sum up the whole dataset
          vce = "HC1") # compute covariance with HC1
  #abline(lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans, data = crossSectionalTimeAveraged), col = "red")
  ggsave(file = paste(dateToUse,"binsregLIQRAT.png", sep = "")) # save fig
  dev.off()
  
  # compute the scatter plot to compare the gain 
  png(file = paste(dateToUse,"scatterLIQRAT.png", sep = ""))
  plot(crossSectionalTimeAveraged$LIQRAT, crossSectionalTimeAveraged$DEPRAT, 
       xlab="DEPRAT", ylab="LIQRAT", pch=19, col = "blue", cex = 0.2)
  dev.off()
  
  # SECRAT
  binsreg(y = SECRAT, x= DEPRAT
          , w = ~ASSET + ciloans + persloans + reloans,
          data = as.data.frame( crossSectionalTimeAveraged),
          nbins = 50, # use 50 point to sum up the whole dataset
          vce = "HC1") # compute covariance with HC3
  ggsave(file = paste(dateToUse,"binsregSECRAT.png", sep = "")) # save fig
  dev.off()
  
  # compute the scatter plot to compare the gain 
  png(file = paste(dateToUse,"scatterSECRAT.png", sep = ""))
  plot(crossSectionalTimeAveraged$SECRAT, crossSectionalTimeAveraged$DEPRAT, 
       xlab="DEPRAT", ylab="SECRAT", pch=19, col = "blue", cex = 0.2)
  dev.off()
  
  # COMRAT
  binsreg(y = COMRAT, x= DEPRAT
          , w = ~ASSET + ciloans + persloans + reloans,
          data = as.data.frame( crossSectionalTimeAveraged),
          nbins = 50, # use 50 point to sum up the whole dataset
          vce = "HC1") # compute covariance with HC3
  ggsave(file = paste(dateToUse,"binsregCOMRAT.png", sep = "")) # save fig
  dev.off()
  
  # compute the scatter plot to compare the gain 
  png(file = paste(dateToUse,"scatterCOMRAT.png", sep = ""))
  plot(crossSectionalTimeAveraged$COMRAT, crossSectionalTimeAveraged$DEPRAT, 
       xlab="DEPRAT", ylab="COMRAT", pch=19, col = "blue", cex = 0.2)
  dev.off()
  
  ######### Q4 #########
  # construct panel datq with the 600 largest banks based on the bhcid in big ans mid bank category from above
  panel600 = dt[ dt$bhcid %in% c( unique( cstaBig$bhcid ), unique( cstaMid$bhcid ) ), ] %>% na.omit()
  panelSmall = dt[ dt$bhcid %in% unique( cstaSmall$bhcid ), ] %>% na.omit() # remaining banks 
  rm(crossSectionalTimeAveraged, cstaBig, cstaMid, cstaSmall)
  
  # construct the dataset for 600 and smallest banks with the largest at each date, the issue with that is that it is not comparable
  #with above 
  #panelBigMidVar = dt %>%
  #  group_by( date ) %>%
  #  arrange( desc( ASSET ) ) %>%
  #  slice( 1:600 ) %>%
  #  ungroup() 
  
  #panelSmallVar = dt %>%
  #  group_by( date ) %>%
  #  arrange( desc( ASSET ) ) %>%
  #  slice( 601:nbrOfFirms ) %>%
  #  ungroup()
  
  # they dont have the same number of observations. why ? former we said : based on the average select the 600 largest, then
  # get all the corresponding data. but some banks dont have 20 date observations which reduce the total number. The latter, 
  # at each date, we measure the 600 largest and keep only these value so obviously we have more date. 
  
  # run regressions with fixed effects  using feols from fixest package
  # regress LIQRAT on DEPRAT with control variables and bhcid as fixed effect  on the 600 largest banks
  m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
             data = panel600 )
  etable(m1, file = paste("Output/", dateToUse,"largeBankLIQRATBhcid.tex", sep = "")) 
  res1 = summaryfeols(m1, panel600, c("DEPRAT", "LIQRAT"), "Large Banks") # same as for time averaged reg but adapted to the output of feols

  m2 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
             data = panel600 ) # same as above except that FE are date
  etable(m2, file = paste("Output/", dateToUse,"largeBankLIQRATDate.tex", sep = ""))
  res2 = summaryfeols(m2, panel600, c("DEPRAT", "LIQRAT"), "Large Banks")

  m3 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
             data = panel600 ) # same regression except that FE are bhcid and date
  etable(m3, file = paste("Output/", dateToUse,"largeBankLIQRATBhcidDate.tex", sep = ""))
  res3 = summaryfeols(m3, panel600, c("DEPRAT", "LIQRAT"), "Large Banks")

  # LIQRAT small
  m4 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
             data = panelSmall )
  etable(m3, file = paste("Output/", dateToUse,"smallBankLIQRATBhcid.tex", sep = ""))
  res4 = summaryfeols(m3, panelSmall, c("DEPRAT", "LIQRAT"), "Small Banks")

  m5 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
             data = panelSmall )
  etable(m5, file = paste("Output/", dateToUse,"smallBankLIQRATDate.tex", sep = ""))
  res5 = summaryfeols(m5, panelSmall, c("DEPRAT", "LIQRAT"), "Small Banks")

  m6 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
             data = panelSmall )
  etable(m6, file = paste("Output/", dateToUse,"smallBankLIQRATBhcidDate.tex", sep = ""))
  res6 = summaryfeols(m6, panelSmall, c("DEPRAT", "LIQRAT"), "Small Banks")

  etable(m1,m2, m3, m4, m5, m6, file = paste("Output/", dateToUse, "resumeFELargeSmallLIQRAT.tex", sep = "")) # save outpout of multiple reg
  tabIIIba = cbind(rbind(res1, res2, res3), rbind(res4, res5, res6)) # table of results with beta coeff, tstat and explanatory power
  
  #SECRAT large, change the dependent variables here it is SECRAT
  m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
             data = panel600 )
  etable(m1, file = paste("Output/", dateToUse,"LargeBankSECRATBhcid.tex", sep = ""))
  res1 = summaryfeols(m1, panel600, c("DEPRAT", "SECRAT"), "Large Banks")
  
  m2 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
             data = panel600 )
  etable(m2, file = paste("Output/", dateToUse,"LargeBankSECRATDate.tex", sep = ""))
  res1 = summaryfeols(m1, panel600, c("DEPRAT", "SECRAT"), "Large Banks")
  
  m3 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
             data = panel600 )
  etable(m3, file = paste("Output/", dateToUse,"LargeBankSECRATBhcidDate.tex", sep = ""))
  res3 = summaryfeols(m1, panel600, c("DEPRAT", "SECRAT"), "Large Banks")
  
  #SECRAT small
  m4 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
             data = panelSmall )
  etable(m4, file = paste("Output/", dateToUse,"SmallBankSECRATBhcid.tex", sep = ""))
  res4 = summaryfeols(m1, panelSmall, c("DEPRAT", "SECRAT"), "Small Banks")
  
  m5 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
             data = panelSmall )
  etable(m5, file = paste("Output/", dateToUse,"SmallBankSECRATDate.tex", sep = ""))
  res5 = summaryfeols(m1, panelSmall, c("DEPRAT", "SECRAT"), "Small Banks")
  
  m6 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
             data = panelSmall )
  etable(m6, file = paste("Output/", dateToUse,"SmallBankSECRATBhcidDate.tex", sep = ""))
  res6 = summaryfeols(m1, panelSmall, c("DEPRAT", "SECRAT"), "Small Banks")

  tabIIIbb = cbind(rbind(res1, res2, res3), rbind(res4, res5, res6))
  tabIIIFE = rbind(tabIIIba, tabIIIbb)
  
  etable(m1,m2, m3, m4, m5, m6, file = paste("Output/", dateToUse, "resumeFELargeSmallSECRAT.tex", sep = ""))
  
  rm(tabIIIba, tabIIIbb)
  
  #COMRAT large, change the dependent variable here it is COMRAT
  m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
             data = panel600 )
  etable(m1, file = paste("Output/", dateToUse,"LargeBankCOMRATBhcid.tex", sep = ""))
  res1 = summaryfeols(m1, panel600, c("DEPRAT", "COMRAT"), "Large Banks")

  m2 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
             data = panel600 )
  etable(m2, file = paste("Output/", dateToUse,"LargeBankCOMRATDate.tex", sep = ""))
  res2 = summaryfeols(m2, panel600, c("DEPRAT", "COMRAT"), "Large Banks")

  m3 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
             data = panel600 )
  etable(m3, file = paste("Output/", dateToUse,"LargeBankCOMRATBhcidDate.tex", sep = ""))
  res3 = summaryfeols(m3, panel600, c("DEPRAT", "COMRAT"), "Large Banks")
  
  #COMRAT small
  m4 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
             data = panelSmall)
  etable(m4, file = paste("Output/", dateToUse,"SmallBankCOMRATBhcid.tex", sep = ""))
  res4 = summaryfeols(m4, panelSmall, c("DEPRAT", "COMRAT"), "Small Banks")

  m5 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
             data = panelSmall)
  etable(m5, file = paste("Output/", dateToUse,"SmallBankCOMRATDate.tex", sep = ""))
  res5 = summaryfeols(m5, panelSmall, c("DEPRAT", "COMRAT"), "Small Banks")

  m6 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
             data = panelSmall)
  etable(m6, file = paste("Output/", dateToUse,"SmallBankCOMRATBhcidDate.tex", sep = ""))
  res6 = summaryfeols(m6, panelSmall, c("DEPRAT", "COMRAT"), "Small Banks")

  etable(m1,m2, m3, m4, m5, m6, file = paste("Output/", dateToUse, "resumeFELargeSmallCOMRAT.tex", sep = ""))
  tabIVb = cbind(rbind(res1, res2, res3), rbind(res4, res5, res6))
  
  result = list(tabI, tabIII, tabIIINoControl, tabIIIFE, tabIV, tabIVNoControl, tabIVb)
  return(result)
}
