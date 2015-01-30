 index<-function(from,to) {
  #indexing help function for R.
  #Similar to matlabs ':', where 3:1 = c() instead of c(3,2,1) of R.

  if ( from > to ) {
    R<-c()
  } else {
    R<-from:to
  }
  R
}
