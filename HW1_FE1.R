library(data.table) 
library(readxl)
library( plm )
library( dplyr )

# author Maxime Borel

pnl<- read.csv( "PS1_data/callreports_1976_2021wc.csv" ) # read the data
pnl$date = strptime(pnl$date, format = "%Y%m%d")
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype", "fedfundsrepoasset", "securities",
                                                              "assets","cash", "transdep", "deposits", "commitments", "loans")]

chartertype = unique(df9296$chartertype)

x = filter(df9296, chartertype==200 | chartertype==300 | chartertype==320 | chartertype==340) # filter data to keep only the one with a chartertype of 200

x$bhcid[x$bhcid==0] = x$rssdid[x$bhcid==0] #need to identify by holding for aggregating
y = x %>% # aggregate bank that share the same bhic and sum it to get the holding amount
  group_by(bhcid, date ) %>%
  summarise(fedfundsrepoasset=sum(fedfundsrepoasset), securities=sum(securities), assets=sum(assets),
            cash=sum(cash), transdep=sum(transdep),  deposits=sum(deposits),  commitments=sum(commitments),  loans=sum(loans))%>%
  ungroup()%>%
  as.data.frame()

z = y %>% #remove every bank that has less than 8 observations
  group_by(bhcid) %>%
  filter(n()>=8) %>%
  ungroup()

nbrOfFirms = length(unique(z$bhcid))

SECRAT <- (z$fedfundsrepoasset + z$securities)/z$assets
LIQRAT <- SECRAT + z$cash/z$assets
DEPRAT <- z$transdep / z$deposits
COMRAT <- z$commitments/(z$commitments+z$loans)

df = data.frame( list( z$bhcid, z$date, LIQRAT, SECRAT, DEPRAT, COMRAT, z$assets ) )
colnames( df ) = c( "bhcid", "date", "LIQRAT", "SECRAT", "DEPRAT", "COMRAT", "ASSET" )

tableI = df %>%
  group_by(date)%>%
  summarise(mLIQRAT=median(LIQRAT, na.rm = TRUE), mSECRAT=median(SECRAT, na.rm = TRUE), mDEPRAT=median(DEPRAT, na.rm = TRUE), mCOMRAT=median(COMRAT, na.rm = TRUE),
            q1LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.25), q1SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.25),
            q1DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.25), q1COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.25),
            q3LIQRAT=quantile(LIQRAT, na.rm = TRUE, probs = 0.75), q3SECRAT=quantile(SECRAT, na.rm = TRUE, probs = 0.75),
            q3DEPRAT=quantile(DEPRAT, na.rm = TRUE, probs = 0.75), q3COMRAT=quantile(COMRAT, na.rm = TRUE, probs = 0.75))%>%
  ungroup()%>%
  as.data.frame()

table1 = as.data.frame(colMeans(tableI[,-1]))

temp

med = aggregate(df$LIQRAT, list(df$date), FUN=median, na.rm=TRUE)
mean(med[,2])
quant = aggregate(df$LIQRAT, list(df$date), FUN=quantile, probs=c(0.25, 0.75), na.rm=TRUE)


