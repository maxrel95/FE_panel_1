library(data.table) 
library(readxl)
library( plm )
library( dplyr )

getSampleOnVariable <- function(df, a, b){
  dt = df %>%
    as.data.frame() %>%
    arrange(desc(mean(ASSET))) %>%
    group_by(date) %>%
    slice(a:b)
  dt = dt[order(dt$bhcid),]
  return(dt)
}

quartileTable<- function(df){
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
  return(table1)
}

summaryOLS = function( model, data, coeff, var, name ){
  resume = summary( model )
  res = resume$coefficients[coeff, c("Estimate", "t value")]
  res['Explanatory power'] = res["Estimate"]*( sd( data %>% select(var[1]) %>% unlist(),
                                                   na.rm = TRUE ) / sd( data %>% select(var[2]) %>% unlist(),
                                                                        na.rm = TRUE ) )
  res = as.data.frame( res )
  colnames(res) = name
  return(res)
}
