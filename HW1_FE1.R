library(data.table) 
library(readxl)
library( plm )
library( dplyr )
library(binsreg)
library(ggplot2)
library(fixest)
library(stargazer)
library(starpolishr)
source("All_functions.R")

load("~/Documents/Université/HEC/PhD/6.1/FE I/HW3/.RData")

result1 = banksAsLiquidityProvider( dateStart = "1992-03-31", dateEnd = "1996-12-31" ) 
result2 = banksAsLiquidityProvider( dateStart = "1997-03-31", dateEnd = "2013-12-31" ) 

# author Maxime Borel

pnl<- read.csv( "PS1_data/callreports_1976_2021wc.csv" ) # read the data
pnl$date = strptime(pnl$date, format = "%Y%m%d")
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype",
                                                                "fedfundsrepoasset", "securities","assets",
                                                                "cash", "transdep", "deposits", "commitments",
                                                                "loans","ciloans", "persloans", "reloans")]
if (sum(is.na(df9296$bhcid))>0){
  df9296$bhcid[is.na(df9296$bhcid)] = 0
}
df9296$bhcid[df9296$bhcid==0] = df9296$rssdid[df9296$bhcid==0]

# find the all possible chartertype
chartertype = unique(df9296$chartertype)

filteredData = df9296%>%
  na.omit()%>%
  filter(chartertype==200 | chartertype==300 | chartertype==320 | chartertype==340)%>%
  group_by(bhcid, date ) %>%
  summarise(fedfundsrepoasset=sum(fedfundsrepoasset, na.rm = T), securities=sum(securities, na.rm = T), assets=sum(assets, na.rm = T),
            cash=sum(cash, na.rm = T), transdep=sum(transdep, na.rm = T),  deposits=sum(deposits, na.rm = T),
            commitments=sum(commitments, na.rm = T),  loans=sum(loans, na.rm = T),
            ciloans=sum(ciloans, na.rm = T), persloans=sum(persloans, na.rm = T) , reloans=sum(reloans, na.rm = T))%>%
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
  na.omit()%>%
  as.data.frame() %>%
  group_by(bhcid) %>%
  summarise(LIQRAT=mean(LIQRAT, na.rm=TRUE), SECRAT=mean(SECRAT, na.rm=TRUE), DEPRAT=mean(DEPRAT, na.rm=TRUE),
            COMRAT=mean(COMRAT, na.rm=TRUE), ASSET=mean(ASSET, na.rm=TRUE), ciloans=mean(ciloans, na.rm=TRUE),
            persloans=mean(persloans, na.rm=TRUE), reloans=mean(reloans, na.rm=TRUE)) %>%
  ungroup()

nbrOfFirms = length(unique(crossSectionalTimeAveraged$bhcid))


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
rownames(t1) = c("LIQRAT", "SECRAT", "DEPRAT", "COMRAT")
tabI = round(as.data.frame(t1), digits = 3)

rm(df9296, t1)

######### Q2 #########
######## Table III #########
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

#
m1 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
m2 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
m3 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
m4 = lm(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
star.panel1 = stargazer(m1, m2, m3, m4)

m1a = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = crossSectionalTimeAveraged)
m2b = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaBig)
m3c = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaMid)
m4d = lm(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans,
        data = cstaSmall)
star.panel2 = stargazer( m1a, m2b, m3c, m4d)
star.panel.out = star_panel(star.panel1, star.panel2, panel.names = c("LIQRAT", "SECRAT"))
star_tex_write(star.panel.out, file = "tabIIIba.tex", headers = TRUE)

##


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

# remove controle variables
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

rm( m1, res1, res2, res3, res4, tabIIIa, tabIIIb)

######### Q3 ##########
binsreg(y = LIQRAT, x= DEPRAT
        , w = ~ASSET + ciloans + persloans + reloans,
        data = as.data.frame( crossSectionalTimeAveraged),
         nbins = 50,
        vce = "HC3")
ggsave(file="binsreg9296.png")
dev.off()

png(file="scatter9296.png")
plot(crossSectionalTimeAveraged$LIQRAT, crossSectionalTimeAveraged$DEPRAT, 
     xlab="DEPRAT", ylab="LIQRAT", pch=19, col = "blue", cex = 0.2)
dev.off()


######### Q4 #########
# get the 600 largest banks, either I take the 600 largest using the average over time which induce bias in result or at each date, 
# I select the 600 largest which might have more than 600 different bank but still at each date they are the largest 

# construct the dataset with the 600 largest based on average and the samll one
panel600 = dt[ dt$bhcid %in% c( unique( cstaBig$bhcid ), unique( cstaMid$bhcid ) ), ]
panelSmall = dt[ dt$bhcid %in% unique( cstaSmall$bhcid ), ]
rm(crossSectionalTimeAveraged, cstaBig, cstaMid, cstaSmall, filteredData)


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
# LIQRAT large 
m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
        data = panel600 )
etable(m1, file = "largeBankLIQRATBhcid.tex")
res1 = summaryfeols(m1, panel600, 2, c("DEPRAT", "LIQRAT"), "Large Banks")
rm(m1)
m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
         data = panel600 )
etable(m1, file = "largeBankLIQRATDate.tex")
res2 = summaryfeols(m1, panel600, 2, c("DEPRAT", "LIQRAT"), "Large Banks")
rm(m1)
m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
          data = panel600 )
etable(m1, file = "largeBankLIQRATBhcidDate.tex")
res3 = summaryfeols(m1, panel600, 2, c("DEPRAT", "LIQRAT"), "Large Banks")
rm(m1)

# LIQRAT small
m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
          data = panelSmall )
etable(m1, file = "smallBankLIQRATBhcid.tex")
res4 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "LIQRAT"), "Small Banks")
rm(m1)
m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
          data = panelSmall )
etable(m1, file = "smallBankLIQRATDate.tex")
res5 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "LIQRAT"), "Small Banks")
rm(m1)
m1 = feols(LIQRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
          data = panelSmall )
etable(m1, file = "smallBankLIQRATBhcidDate.tex")
res6 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "LIQRAT"), "Small Banks")
rm(m1)

tabIIIba = cbind(rbind(res1, res2, res3), rbind(res4, res5, res6))

#SECRAT large
m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
        data = panel600 )
etable(m1, file = "LargeBankSECRATBhcid.tex")
res1 = summaryfeols(m1, panel600, 2, c("DEPRAT", "SECRAT"), "Large Banks")
rm(m1)
m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
        data = panel600 )
etable(m1, file = "LargeBankSECRATDate.tex")
res1 = summaryfeols(m1, panel600, 2, c("DEPRAT", "SECRAT"), "Large Banks")
rm(m1)
m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
        data = panel600 )
etable(m1, file = "LargeBankSECRATBhcidDate.tex")
res3 = summaryfeols(m1, panel600, 2, c("DEPRAT", "SECRAT"), "Large Banks")
rm(m1)

#SECRAT small
m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
        data = panelSmall )
etable(m1, file = "SmallBankSECRATBhcid.tex")
res4 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "SECRAT"), "Small Banks")
rm(m1)

m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
        data = panelSmall )
etable(m1, file = "SmallBankSECRATDate.tex")
res5 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "SECRAT"), "Small Banks")
rm(m1)

m1 = feols(SECRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
        data = panelSmall )
etable(m1, file = "SmallBankSECRATBhcidDate.tex")
res6 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "SECRAT"), "Small Banks")
rm(m1)

tabIIIbb = cbind(rbind(res1, res2, res3), rbind(res4, res5, res6))
tabIIIFE = rbind(tabIIIba, tabIIIbb)
rm(tabIIIba, tabIIIbb)

#COMRAT large
m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
        data = panel600 )
etable(m1, file = "LargeBankCOMRATBhcid.tex")
res1 = summaryfeols(m1, panel600, 2, c("DEPRAT", "COMRAT"), "Large Banks")
rm(m1)
m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
        data = panel600 )
etable(m1, file = "LargeBankCOMRATDate.tex")
res2 = summaryfeols(m1, panel600, 2, c("DEPRAT", "COMRAT"), "Large Banks")
rm(m1)
m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
        data = panel600 )
etable(m1, file = "LargeBankCOMRATBhcidDate.tex")
res3 = summaryfeols(m1, panel600, 2, c("DEPRAT", "COMRAT"), "Large Banks")
rm(m1)

#COMRAT small
m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid,
        data = panelSmall)
etable(m1, file = "SmallBankCOMRATBhcid.tex")
res4 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "COMRAT"), "Small Banks")
rm(m1)
m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | date,
        data = panelSmall)
etable(m1, file = "SmallBankCOMRATDate.tex")
res5 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "COMRAT"), "Small Banks")
rm(m1)
m1 = feols(COMRAT ~ DEPRAT + ASSET + ciloans + persloans + reloans | bhcid + date,
        data = panelSmall)
etable(m1, file = "SmallBankCOMRATBhcidDate.tex")
res6 = summaryfeols(m1, panelSmall, 2, c("DEPRAT", "COMRAT"), "Small Banks")
rm(m1)
tabIVb = cbind(rbind(res1, res2, res3), rbind(res4, res5, res6))
