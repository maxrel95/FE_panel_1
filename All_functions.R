library(data.table) 
library(readxl)
library( plm )
library( dplyr )

quartileTable<- function(df, a, b){
  if(b=="max"){
    b=length(df$ASSET)
  }
  tableI = df %>%
    arrange(desc(ASSET)) %>%
    group_by(date) %>%
    slice(a:b)%>%
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
