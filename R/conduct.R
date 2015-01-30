conduct<- function(test_function,test_data,
                   write_function,write_data,
                   maxcset=Inf,
                   append=TRUE,output=TRUE) {    
  #Function for conducting all independence tests for one data set.
  # test_function - the function conducting the test, either
  #                 test.classic, test.oracle, test.bayes
  # write_function - function that writes the test results in the needed format..
  # maxcset - the maximum conditioning set size.
  # append - whether to append in the file (in case of several data set, we 
  #           do not want to append)
  # verbose - how much information to print.
    
  if ( output ) sink( paste('./../tmp/','pipeline.ind',sep=''), append=append )
  
  for (csetsize in index(0,maxcset) ) { #go from simpler to more complicated tests       
    for ( i in 1:n ) {
      for ( j in 1:n ) {
        if ( i >= j ) next
          
        #start with empty set
        csetvec <- rep( 0, n)
        csetvec[index(1,csetsize)]<-1
            
        while ( !any(is.na(csetvec) ) ) {
          if ( csetvec[i] == 0 && csetvec[j] == 0 ) { #only if neither x and y are cond.
            cset<-bin.to.dec(rev(csetvec))
            
            #calling the test function
            test_result<-test_function(c(i,j),which(csetvec==1),test_data)
            
            #put some parameters right
            test_result$J<-write_data$J
            test_result$jset<-write_data$jset
            test_result$cset<-bin.to.dec(rev(csetvec))
            test_result$M<-setdiff((1:global_n),c(test_result$vars,test_result$C))
            test_result$mset<-getm( test_result$vars, test_result$C)
            #cat(paste(test_result$M,collapse=','),'=',test_result$mset,'\n')
            
            #calling the write function
            if ( output) write_function(test_result,write_data)
            #if ( !output) 
            
            #adding the test result also to global_indeps vector
            global_indeps[[length(global_indeps)+1]]<<-test_result
          } #if x and y are not in the conditioning set
          
         #consider next csetvec given by the following function
         csetvec<-next_colex_comb(csetvec)
        } #while csetvec != NA
      } #for j
    } #for i 
  } # for csetsize

  if (output) sink()
}