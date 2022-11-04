library(data.table) 
library( dplyr )
library(binsreg)
library(ggplot2)
library(fixest)
library(stargazer)
source("All_functions.R")

#load("~/Documents/Université/HEC/PhD/6.1/FE I/HW3/lastRun.RData")

# I built a function that takes as input the period of interest. The user need to provide the starting date
# and the end date of the analysis period. You can see the rest of the code in the file "All_functions.R"
result1 = banksAsLiquidityProvider( dateStart = "1992-03-31", dateEnd = "1996-12-31" ) 
result2 = banksAsLiquidityProvider( dateStart = "1997-03-31", dateEnd = "2013-12-31" )

pnl <- as.data.frame(fread( "PS1_data/callreports_1976_2021wc.csv" )) # read the data

# indicate to the machine that the vector date is actually date datatype
pnl$date = strptime(pnl$date, format = "%Y%m%d")

# slice the data and keep only the period under study and the variables of interest
df9296 = pnl[pnl$date>="1992-03-31" & pnl$date<="1996-12-31", c("date", "rssdid", "bhcid", "chartertype",
                                                        "fedfundsrepoasset", "securities","assets",
                                                        "cash", "transdep", "deposits", "commitments",
                                                        "loans","ciloans", "persloans", "reloans")]

# put some zero where the bhcid is missing
if (sum(is.na(df9296$bhcid))>0){
  df9296$bhcid[is.na(df9296$bhcid)] = 0
}

temp = df9296 %>%
  group_by(rssdid)%>%
  filter(length(unique(bhcid))>2)

shiftTable1 = as.data.frame(result2[1]) - as.data.frame(result1[1])
shiftTable2 = as.data.frame(result2[2]) - as.data.frame(result1[2])

effectControleT3 = as.data.frame(result1[3]) - as.data.frame(result1[2])
