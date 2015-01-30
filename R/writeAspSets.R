writeAspSets<-function(n,file=NA) {
  #writes down the set definitions for the older encoding.
  #Predicates and "cset", "jset" and "ismember".
  if ( !is.na(file) ) {
    sink(file)
  }
  
  cat('node(1..',n,').\n',sep='')
  #only write the necessary sets..
  jset_written<-rep(FALSE,2^n) 
  cset_written<-rep(FALSE,2^n)
  
  
  #cat('set(0..',2^n-1,').\n',sep='')
  for ( indep in global_indeps ) {
    #going through the global indeps
    if ( !cset_written[indep$cset+1] ) {
      #writing cset only when it is needed
      cat('cset(',indep$cset,'). ',sep='')
      for ( el in indep$C) {
        cat('ismember(',indep$cset,',',el,'). ',sep='')        
      }
      cat('\n')
      cset_written[indep$cset+1]<-TRUE
    }
    
    if ( !jset_written[indep$jset+1] ) {
      cat('jset(',indep$jset,'). ',sep='')        
      for ( el in indep$J) {
        cat('ismember(',indep$jset,',',el,'). ',sep='')        
      }
      cat('\n')
      jset_written[indep$jset+1]<-TRUE      
    }
    
  }#for indep
  cat('\n')
  
  if ( !is.na(file) ) {
    sink() #take out the sink
  }
}


