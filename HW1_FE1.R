library(data.table) 
library(readxl)
library( plm )
library( dplyr )
library(binsreg)
source("All_functions.R")

# author Maxime Borel

pnl<- read.csv( "PS1_data/callreports_1976_2021wc.csv" ) # read the data
pnl$date = strptime(pnl$date, format = "%Y%m%d")
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype", "fedfundsrepoasset", "securities",
                                                              "assets","cash", "transdep", "deposits", "commitments", "loans",
                                                              "ciloans", "persloans", "reloans")]

df9296$bhcid[df9296$bhcid==0] = df9296$rssdid[df9296$bhcid==0]

# find the all possible chartertype
chartertype = unique(df9296$chartertype)

filteredData = df9296%>%
  filter(chartertype==200 | chartertype==300 | chartertype==320 | chartertype==340)%>%
  group_by(bhcid, date ) %>%
  summarise(fedfundsrepoasset=sum(fedfundsrepoasset), securities=sum(securities), assets=sum(assets),
            cash=sum(cash), transdep=sum(transdep),  deposits=sum(deposits),  commitments=sum(commitments),  loans=sum(loans),
            ciloans=sum(ciloans), persloans=sum(persloans) , reloans=sum(reloans))%>%
  ungroup()%>%
  group_by(bhcid) %>%
  filter(n()>=8) %>%
  ungroup()

nbrOfFirms = length(unique(filteredData$bhcid))

filteredData$date = as.POSIXct(filteredData$date)
dt = data.table()
dt[, bhcid:=filteredData$bhcid]
dt[, date:=filteredData$date]
dt[, SECRAT:=(filteredData$fedfundsrepoasset + filteredData$securities)/filteredData$assets]
dt[, LIQRAT:=SECRAT + filteredData$cash/filteredData$assets]
dt[, DEPRAT:=filteredData$transdep / filteredData$deposits]
dt[, COMRAT:=filteredData$commitments/(filteredData$commitments+filteredData$loans)]
dt[, ASSET:=filteredData$assets]
dt[, ciloans:=filteredData$ciloans/filteredData$loans]
dt[, persloans:=filteredData$persloans/filteredData$loans]
dt[, reloans:=filteredData$reloans/filteredData$loans]

dfFull = as.data.frame(dt)
dfBig = getSampleOnVariable(dt, 1, 100)
dfMid = getSampleOnVariable(dt, 101, 600)
dfSmall = getSampleOnVariable(dt, 601, nbrOfFirms)

crossSectionalTimeAveraged = dfFull %>%
  group_by(bhcid) %>%
  summarise(LIQRAT=mean(LIQRAT, na.rm=TRUE), SECRAT=mean(SECRAT, na.rm=TRUE), DEPRAT=mean(DEPRAT, na.rm=TRUE),
            COMRAT=mean(COMRAT, na.rm=TRUE), ASSET=mean(ASSET, na.rm=TRUE), ciloans=mean(ciloans, na.rm=TRUE),
            persloans=mean(persloans, na.rm=TRUE), reloans=mean(reloans, na.rm=TRUE)) %>%
  ungroup()

cstaBig = crossSectionalTimeAveraged %>%
  arrange(desc(ASSET)) %>%
  slice(1:100)
#cstaBig = subset(cstaBig, select = -c(ASSET))
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
t1 = as.data.frame(t1)

######### Q2
crossSectionalTimeAveraged$ASSET = log(crossSectionalTimeAveraged$ASSET)
cstaBig$ASSET = log(cstaBig$ASSET)
cstaMid$ASSET = log(cstaMid$ASSET)
cstaSmall$ASSET = log(cstaSmall$ASSET)

#######################
m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
m1NoControls = lm(LIQRAT ~ DEPRAT,
        data = crossSectionalTimeAveraged)

m2 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
m2NoControls = lm(SECRAT ~ DEPRAT,
                  data = crossSectionalTimeAveraged)
####################
m3 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
m3NoControls = lm(LIQRAT ~ DEPRAT,
                  data = cstaBig)

m4 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
m4NoControls = lm(SECRAT ~ DEPRAT,
                  data = cstaBig)
####################
m5 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
m5NoControls = lm(LIQRAT ~ DEPRAT,
                  data = cstaMid)

m6 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
m6NoControls = lm(SECRAT ~ DEPRAT,
                  data = cstaMid)
####################
m7 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
m7NoControls = lm(LIQRAT ~ DEPRAT,
                  data = cstaSmall)

m8 = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
m8NoControls = lm(SECRAT ~ DEPRAT,
                  data = cstaSmall)

######### Q3
x = binsreg(crossSectionalTimeAveraged$LIQRAT, crossSectionalTimeAveraged$DEPRAT)







