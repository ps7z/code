test.classic<-function(vars,C,test_data) {
  #Function for conduction one independence test.
  #vars      - variables used to test the independence
  #C         - conditining set as a list of variable indexes
  #test_data - data for the test:  
  #            test_data$N sample size
  #            test_data$p_threshold p-value threshold used
  #            test_data$Cx the covariance matrix used  
  
  test_result<-list()
  test_result$vars<-sort(vars)
  test_result$C<-C
  
  #isIndependent returns the p-value
  test_result$p<-isIndependent( test_data$Cx, vars[1], vars[2], given=C, test_data$N );
  #if it is bigger than the result we have independence
  test_result$independent <- ( test_result$p > test_data$p_threshold )
  
  #weight is always 1
  test_result$w<-1
  
  test_result
}