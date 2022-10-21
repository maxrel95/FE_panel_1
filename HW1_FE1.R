library(data.table) 
library(readxl)
library( plm )
library( dplyr )
source("All_functions.R")

# author Maxime Borel

pnl<- read.csv( "PS1_data/callreports_1976_2021wc.csv" ) # read the data
pnl$date = strptime(pnl$date, format = "%Y%m%d")
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype", "fedfundsrepoasset", "securities",
                                                              "assets","cash", "transdep", "deposits", "commitments", "loans")]

df9296$bhcid[df9296$bhcid==0] = df9296$rssdid[df9296$bhcid==0]

# find the all possible chartertype
chartertype = unique(df9296$chartertype)

filteredData = df9296%>%
  filter(chartertype==200 | chartertype==300 | chartertype==320 | chartertype==340)%>%
  group_by(bhcid, date ) %>%
  summarise(fedfundsrepoasset=sum(fedfundsrepoasset), securities=sum(securities), assets=sum(assets),
            cash=sum(cash), transdep=sum(transdep),  deposits=sum(deposits),  commitments=sum(commitments),  loans=sum(loans))%>%
  ungroup()%>%
  group_by(bhcid) %>%
  filter(n()>=8) %>%
  ungroup()

nbrOfFirms = length(unique(filteredData$bhcid))

SECRAT <- (filteredData$fedfundsrepoasset + filteredData$securities)/filteredData$assets
LIQRAT <- SECRAT + filteredData$cash/filteredData$assets
DEPRAT <- filteredData$transdep / filteredData$deposits
COMRAT <- filteredData$commitments/(filteredData$commitments+filteredData$loans)

df = data.frame( list( filteredData$bhcid, filteredData$date, LIQRAT, SECRAT, DEPRAT, COMRAT, filteredData$assets ) )
colnames( df ) = c( "bhcid", "date", "LIQRAT", "SECRAT", "DEPRAT", "COMRAT", "ASSET" )

test = quartileTable(df, 1, 'max')

tableI = df %>%
  arrange(desc(ASSET)) %>%
  group_by(date) %>%
  slice(1:length(ASSET))%>%
  group_by(date)%>%
  summarise(mLIQRAT=median(LIQRAT, na.rm = TRUE), mSECRAT=median(SECRAT, na.rm = TRUE), mDEPRAT=median(DEPRAT, na.rm = TRUE), mCOMRAT=median(COMRAT, na.rm = TRUE),
            q1LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.25), q1SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.25),
            q1DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.25), q1COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.25),
            q3LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.75), q3SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.75),
            q3DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.75), q3COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.75))%>%
  ungroup()%>%
  as.data.frame()
table1 = as.data.frame(colMeans(tableI[,-1]))

test2 = quartileTable(df, 1, 100)

table12 = df %>%
  arrange(desc(ASSET)) %>%
  group_by(date) %>%
  slice(1:100)%>%
  summarise(mLIQRAT=median(LIQRAT, na.rm = TRUE), mSECRAT=median(SECRAT, na.rm = TRUE), mDEPRAT=median(DEPRAT, na.rm = TRUE), mCOMRAT=median(COMRAT, na.rm = TRUE),
            q1LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.25), q1SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.25),
            q1DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.25), q1COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.25),
            q3LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.75), q3SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.75),
            q3DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.75), q3COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.75))%>%
  ungroup()%>%
  as.data.frame()
table12 = as.data.frame(colMeans(table12[,-1]))

table13 = df %>%
  arrange(desc(ASSET)) %>%
  group_by(date) %>%
  slice(101:600)%>%
  summarise(mLIQRAT=median(LIQRAT, na.rm = TRUE), mSECRAT=median(SECRAT, na.rm = TRUE), mDEPRAT=median(DEPRAT, na.rm = TRUE), mCOMRAT=median(COMRAT, na.rm = TRUE),
            q1LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.25), q1SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.25),
            q1DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.25), q1COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.25),
            q3LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.75), q3SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.75),
            q3DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.75), q3COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.75))%>%
  ungroup()%>%
  as.data.frame()
table13 = as.data.frame(colMeans(table13[,-1]))

table14 = df %>%
  arrange(desc(ASSET)) %>%
  group_by(date) %>%
  slice(601:length(ASSET))%>%
  summarise(mLIQRAT=median(LIQRAT, na.rm = TRUE), mSECRAT=median(SECRAT, na.rm = TRUE), mDEPRAT=median(DEPRAT, na.rm = TRUE), mCOMRAT=median(COMRAT, na.rm = TRUE),
            q1LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.25), q1SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.25),
            q1DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.25), q1COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.25),
            q3LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.75), q3SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.75),
            q3DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.75), q3COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.75))%>%
  ungroup()%>%
  as.data.frame()
table14 = as.data.frame(colMeans(table14[,-1]))



