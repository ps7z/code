test_quality<-function(M) {
  #Checks the quality of the tests against true model M.
  #different proper scores
  logloss<-c()
  quadloss<-c()
  #plain binary score
  binloss<-c()
  true_independent<-c()
  test_independent<-c()
  p<-c() #gathering all probabilities/p-values for some comparisions
  str<-c()
  value<-0
  for ( indep in global_indeps) {#go throgh all 
    #the true result!
    independent<-!directed_reachable(indep$vars[1],indep$vars[2],indep$C,indep$J,M)
    #indep$p includes the probability the test tought to be the probability of indep
    #logloss is the negative logarithm assigned to the true class
    if ( independent ) {
      logloss<-c(logloss,-log(indep$p))
      quadloss<-c(quadloss, sum( (c(indep$p,1-indep$p) - c(1,0) )^2 ) )
    } else {
      logloss<-c(logloss,-log(1-indep$p))      
      quadloss<-c(quadloss, sum( (c(indep$p,1-indep$p) - c(0,1) )^2 ) )
    }
    binloss<-c(binloss, 1*( indep$independent != independent) )
    true_independent<-c(true_independent,1*independent) #is truly ind.
    test_independent<-c(test_independent,1*indep$independent)#tests independent?
    
    p<-c(p,indep$p)#collect p-values, or probabilities
    symbol<-'_||_'
    if (!independent) symbol<-'_N_'
    var_names<-c('x','y','z','w')
    stri<-paste(var_names[indep$vars[1]],symbol,var_names[indep$vars[2]],sep='')
    if ( length(indep$C) != 0 ) stri<-paste(stri,'|',paste(var_names[indep$C],collapse=','),sep='')
    str<-c(str,stri)
  }
  
  list(binloss=binloss,test_independent=test_independent,
       true_independent=true_independent,p=p,str=str)
}