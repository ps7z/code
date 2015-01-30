isIndependent <-function( C, x, y, given=NULL, N ) {
  # Returns a p-value of an independence test.
  #INPUT:
  # C         - Covariance matrix of the data.
  # x,y       - Indexes of variables in question.
  # given     - A vector of variable indexes that are conditioned on.
  # N         - The number of samples.
  #OUTPUT:
  # p-value of the test
 #browser()
  #calculating first the partial correlation
  p <- pcor( c(x,y,given), C)

  #returning a p-value of this test 
  pcor.test(p, length(given), N )$pvalue
}

pcor<-function(u,S) {
  k<-solve(S[u,u])
  -k[1,2]/sqrt(k[1,1]*k[2,2])
}
