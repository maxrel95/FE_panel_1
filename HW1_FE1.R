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

shiftTable1 = as.data.frame(result2[1]) - as.data.frame(result1[1])
shiftTable2 = as.data.frame(result2[2]) - as.data.frame(result1[2])

effectControleT3 = as.data.frame(result1[3]) - as.data.frame(result1[2])
