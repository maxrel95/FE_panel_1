library(data.table) 
library(readxl)
library( plm )
library( dplyr )
library(binsreg)
library(ggplot2)
source("All_functions.R")

# author Maxime Borel

pnl<- read.csv( "PS1_data/callreports_1976_2021wc.csv" ) # read the data
pnl$date = strptime(pnl$date, format = "%Y%m%d")
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype",
                                                                "fedfundsrepoasset", "securities","assets",
                                                                "cash", "transdep", "deposits", "commitments",
                                                                "loans","ciloans", "persloans", "reloans")]

df9296$bhcid[df9296$bhcid==0] = df9296$rssdid[df9296$bhcid==0]

# find the all possible chartertype
chartertype = unique(df9296$chartertype)

filteredData = df9296%>%
  filter(chartertype==200 | chartertype==300 | chartertype==320 | chartertype==340)%>%
  group_by(bhcid, date ) %>%
  summarise(fedfundsrepoasset=sum(fedfundsrepoasset), securities=sum(securities), assets=sum(assets),
            cash=sum(cash), transdep=sum(transdep),  deposits=sum(deposits),
            commitments=sum(commitments),  loans=sum(loans),
            ciloans=sum(ciloans), persloans=sum(persloans) , reloans=sum(reloans))%>%
  ungroup()%>%
  group_by(bhcid) %>%
  filter(n()>=8) %>%
  ungroup() %>%
  mutate( bhcid = as.factor( bhcid),
          date = as.factor( as.character( date ) ) )

nbrOfFirms = length(unique(filteredData$bhcid))

dt = data.table()
dt[, bhcid:=filteredData$bhcid]
dt[, date:=filteredData$date]
dt[, SECRAT:=(filteredData$fedfundsrepoasset + filteredData$securities)/filteredData$assets]
dt[, LIQRAT:=SECRAT + filteredData$cash/filteredData$assets]
dt[, DEPRAT:=filteredData$transdep / filteredData$deposits]
dt[, COMRAT:=filteredData$commitments/(filteredData$commitments+filteredData$loans)]
dt[, ASSET:=log(filteredData$assets)]
dt[, ciloans:=filteredData$ciloans/filteredData$loans]
dt[, persloans:=filteredData$persloans/filteredData$loans]
dt[, reloans:=filteredData$reloans/filteredData$loans]

crossSectionalTimeAveraged = dt %>%
  as.data.frame() %>%
  group_by(bhcid) %>%
  summarise(LIQRAT=mean(LIQRAT, na.rm=TRUE), SECRAT=mean(SECRAT, na.rm=TRUE), DEPRAT=mean(DEPRAT, na.rm=TRUE),
            COMRAT=mean(COMRAT, na.rm=TRUE), ASSET=mean(ASSET, na.rm=TRUE),ciloans=mean(ciloans, na.rm=TRUE),
            persloans=mean(persloans, na.rm=TRUE), reloans=mean(reloans, na.rm=TRUE)) %>%
  ungroup()

cstaBig = crossSectionalTimeAveraged %>%
  arrange(desc(ASSET)) %>%
  slice(1:100)
cstaMid = crossSectionalTimeAveraged %>%
  arrange(desc(ASSET)) %>%
  slice(101:600)
cstaSmall = crossSectionalTimeAveraged %>%
  arrange(desc(ASSET)) %>%
  slice(601:nbrOfFirms)

q = c(0.25, 0.5, 0.75)

t1 = cbind(rbind(quantile(crossSectionalTimeAveraged$LIQRAT, probs = q, na.rm = TRUE),
                quantile(crossSectionalTimeAveraged$SECRAT, probs = q, na.rm = TRUE),
                quantile(crossSectionalTimeAveraged$DEPRAT, probs = q, na.rm = TRUE),
                quantile(crossSectionalTimeAveraged$COMRAT, probs = q, na.rm = TRUE)),
           rbind(quantile(cstaBig$LIQRAT, probs = q, na.rm = TRUE),
                 quantile(cstaBig$SECRAT, probs = q, na.rm = TRUE),
                 quantile(cstaBig$DEPRAT, probs = q, na.rm = TRUE),
                 quantile(cstaBig$COMRAT, probs = q, na.rm = TRUE)),
           rbind(quantile(cstaMid$LIQRAT, probs = q, na.rm = TRUE),
                 quantile(cstaMid$SECRAT, probs = q, na.rm = TRUE),
                 quantile(cstaMid$DEPRAT, probs = q, na.rm = TRUE),
                 quantile(cstaMid$COMRAT, probs = q, na.rm = TRUE)),
           rbind(quantile(cstaSmall$LIQRAT, probs = q, na.rm = TRUE),
                 quantile(cstaSmall$SECRAT, probs = q, na.rm = TRUE),
                 quantile(cstaSmall$DEPRAT, probs = q, na.rm = TRUE),
                 quantile(cstaSmall$COMRAT, probs = q, na.rm = TRUE)))
rownames(t1) = c("Full", "Big", "Mid", "Small")
t1 = round(as.data.frame(t1), digits = 3)

rm(df9296)

######### Q2 #########

m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "All Banks")

m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
res2 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "LIQRAT"), "Large Banks")

m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
res3 = summaryOLS(m1, cstaMid, 2, c("DEPRAT", "LIQRAT"), "Medium Banks")

m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
res4 = summaryOLS(m1, cstaSmall, 2, c("DEPRAT", "LIQRAT"), "Small Banks")

tabIIIa = cbind( res1, res2, res3, res4)


m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "SECRAT"), "All Banks")

m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
res2 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "SECRAT"), "Large Banks")

m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
res3 = summaryOLS(m1, cstaMid, 2, c("DEPRAT", "SECRAT"), "Medium Banks")

m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
res4 = summaryOLS(m1, cstaSmall, 2, c("DEPRAT", "SECRAT"), "Small Banks")

tabIIIb = cbind( res1, res2, res3, res4)

tabIII = rbind( tabIIIa, tabIIIb)

# remove all control variables

m1 = lm(LIQRAT ~ DEPRAT,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "All Banks")

m1 = lm(LIQRAT ~ DEPRAT,
        data = cstaBig)
res2 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "LIQRAT"), "Large Banks")

m1 = lm(LIQRAT ~ DEPRAT,
        data = cstaMid)
res3 = summaryOLS(m1, cstaMid, 2, c("DEPRAT", "LIQRAT"), "Medium Banks")

m1 = lm(LIQRAT ~ DEPRAT,
        data = cstaSmall)
res4 = summaryOLS(m1, cstaSmall, 2, c("DEPRAT", "LIQRAT"), "Small Banks")

tabIIIa = cbind( res1, res2, res3, res4)


m1 = lm(SECRAT ~ DEPRAT,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "SECRAT"), "All Banks")

m1 = lm(SECRAT ~ DEPRAT,
        data = cstaBig)
res2 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "SECRAT"), "Large Banks")

m1 = lm(SECRAT ~ DEPRAT,
        data = cstaMid)
res3 = summaryOLS(m1, cstaMid, 2, c("DEPRAT", "SECRAT"), "Medium Banks")

m1 = lm(SECRAT ~ DEPRAT,
        data = cstaSmall)
res4 = summaryOLS(m1, cstaSmall, 2, c("DEPRAT", "SECRAT"), "Small Banks")

tabIIIb = cbind( res1, res2, res3, res4)

tabIIINoControl = rbind( tabIIIa, tabIIIb)

##### Tab IV #######
m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "COMRAT"), "All Banks")

m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
res2 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "COMRAT"), "Large Banks")

m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
res3 = summaryOLS(m1, cstaMid, 2, c("DEPRAT", "COMRAT"), "Medium Banks")

m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
res4 = summaryOLS(m1, cstaSmall, 2, c("DEPRAT", "COMRAT"), "Small Banks")

tabIV = cbind( res1, res2, res3, res4)

m1 = lm(COMRAT ~ DEPRAT,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "COMRAT"), "All Banks")

m1 = lm(COMRAT ~ DEPRAT,
        data = cstaBig)
res2 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "COMRAT"), "Large Banks")

m1 = lm(COMRAT ~ DEPRAT,
        data = cstaMid)
res3 = summaryOLS(m1, cstaMid, 2, c("DEPRAT", "COMRAT"), "Medium Banks")

m1 = lm(COMRAT ~ DEPRAT,
        data = cstaSmall)
res4 = summaryOLS(m1, cstaSmall, 2, c("DEPRAT", "COMRAT"), "Small Banks")

tabIVNoControl = cbind( res1, res2, res3, res4)















m1 = lm(LIQRAT ~ DEPRAT,
        data = crossSectionalTimeAveraged)
res2 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1NoConrtols")

m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
res3 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m2")

m1 = lm(SECRAT ~ DEPRAT,
                  data = crossSectionalTimeAveraged)
res4 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m2NoConrtols")

####################
m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
res5 = summaryOLS(m1, cstaBig, 2, c("DEPRAT", "LIQRAT"), "m1")
m1 = lm(LIQRAT ~ DEPRAT,
                  data = cstaBig)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")


m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")
m1 = lm(SECRAT ~ DEPRAT,
                  data = cstaBig)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

####################
m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(LIQRAT ~ DEPRAT,
                  data = cstaMid)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(SECRAT ~ DEPRAT,
                  data = cstaMid)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

####################
m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(LIQRAT ~ DEPRAT,
                  data = cstaSmall)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")


m1 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(SECRAT ~ DEPRAT,
                  data = cstaSmall)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

####################
#######################
m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(COMRAT ~ DEPRAT,
                  data = crossSectionalTimeAveraged)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

####################
m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")
m1 = lm(COMRAT ~ DEPRAT,
                  data = cstaBig)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")


####################
m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(COMRAT ~ DEPRAT,
                  data = cstaMid)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")


####################
m1 = lm(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

m1 = lm(COMRAT ~ DEPRAT,
                  data = cstaSmall)
res1 = summaryOLS(m1, crossSectionalTimeAveraged, 2, c("DEPRAT", "LIQRAT"), "m1")

######### Q3
binsreg(crossSectionalTimeAveraged$LIQRAT, crossSectionalTimeAveraged$DEPRAT)

plot(crossSectionalTimeAveraged$LIQRAT, crossSectionalTimeAveraged$DEPRAT, 
     xlab="DEPRAT", ylab="LIQRAT", pch=19, col = "blue", cex = 0.2)

######### Q4
# get the 600 largest banks, either I take the 600 largest using the average over time which induce bias in result or at each date, 
# I select the 600 largest which might have more than 600 different bank but still at each date they are the largest 

panel600 = dt[ dt$bhcid %in% c( unique( cstaBig$bhcid ), unique( cstaMid$bhcid ) ), ]
panelSmall = dt[ dt$bhcid %in% unique( cstaSmall$bhcid ), ]

panelBigMidVar = dt %>%
  group_by( date ) %>%
  arrange( desc( ASSET ) ) %>%
  slice( 1:600 ) %>%
  ungroup() 

panelSmallVar = dt %>%
  group_by( date ) %>%
  arrange( desc( ASSET ) ) %>%
  slice( 601:nbrOfFirms ) %>%
  ungroup()

# they dont have the same number of observations. why ? former we said : based on the average select the 600 largest, then
# get all the corresponding data. but some banks dont have 20 date observations which reduce the total number. The latter, 
# at each date, we measure the 600 largest and keep only these value so obviously we have more date. 

m13a = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid,
        data = panel600)
m13b = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + date,
         data = panel600)
m13c = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid + date,
          data = panel600)

m14a = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid,
          data = panelSmall)
m14b = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + date,
          data = panelSmall)
m14c = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid + date,
          data = panelSmall)

m15a = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid,
          data = panelBigMidVar)
m15b = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + date,
          data = panelBigMidVar)
m15c = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid + date,
          data = panelBigMidVar)

m16a = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid,
          data = panelSmallVar)
m16b = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + date,
          data = panelSmallVar)
m16c = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans + bhcid + date,
          data = panelSmallVar)

