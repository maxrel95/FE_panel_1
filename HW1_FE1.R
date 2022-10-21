library(data.table) 
library(readxl)
library( plm )
library( dplyr )
source("All_functions.R")

# author Maxime Borel

pnl<- read.csv( "PS1_data/callreports_1976_2021wc.csv" ) # read the data
pnl$date = strptime(pnl$date, format = "%Y%m%d")
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype", "fedfundsrepoasset", "securities",
                                                              "assets","cash", "transdep", "deposits", "commitments", "loans",
                                                              "ciloans", "persloans", "reloans", "cert")]

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

t1 = cbind(quartileTable(dfFull),  quartileTable(dfBig), quartileTable(dfMid), quartileTable(dfSmall))
colnames(t1) = c("Full", "Big", "Mid", "Small")

## Q2








