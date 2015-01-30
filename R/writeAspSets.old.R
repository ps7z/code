writeAspSets.old<-function(n,file=NA) {
  #writes down the set definitions as an input file for clingo.
  
  if ( !is.na(file) ) {
    sink(file)
  }
  
  #the nodes
  cat('node(1..',n,').\n',sep='')

  #then the sets, which are indexed from 0 to 2^n-1
  for ( i in index(0,2^n-1) ) {
    ibin<-rev(dec.to.bin(i,n))
    for ( j in index(1,length(ibin)) ) {
      if ( ibin[j] == 1 ) {
        cat('ismember(',i,',',j,'). ',sep='')
      } else {
        #do nothing here
      }
    }
    cat('\n')
  }
  if ( !is.na(file) ) {
    sink() #take out the sink
  }
}