G2st<-function(G) {
  #turns a graph G to a string representation
  st<-c()
  for ( i in 1:nrow(G) ) {
    if ( any(G[i,]==1)) {
      st<-paste(st,'[X',i,'|',paste('X',which(G[i,]==1),collapse=':',sep=''),']',sep=''  )
    } else {
      st<-paste(st,'[X',i,']',sep='')
    }
  }
  st
}