pcor.test<-function(r,q,n) {
  df <- n - 2 - q
  tval<- r*sqrt(df)/sqrt(1-r*r)
  pv <- 2* pt(-abs(tval),df)
  list(tval=tval,df=df,pvalue=pv)
}