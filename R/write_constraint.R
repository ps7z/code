write_constraint<-function(test_result,write_data,verbose=FALSE) {
  #Function for writing the independence constraints.
  #test_result - the result of the test see conduct.
  #write_data$mode   - the asp program file, basically to see whether to 
  #                     use "old" (HHEJ2013) or "new" (HEJ2014) encoding. #
  #write_data$weight - either "constant" or "log".
  #called by "conduct()" function.
  if ( is.na(write_data$mode) ) return
  x<-min(test_result$vars)
  y<-max(test_result$vars)

  #this is to make sure no jset is read from write_data
  write_data$jset<-NA
  
  if ( write_data$weight=="constant") {
    weight<-'1'
  } else if ( write_data$weight=="log" ) {
    weight<-format(round(1000*test_result$w),scientific=FALSE)
  }
    
  if ( grepl('old_',write_data$mode) ) { #if using an old encoding no m set is printed.
    if ( test_result$independent ) {
      cat('indep(',x,',',y,',',test_result$cset,',',test_result$jset,
              ',',weight,'). ',
            '%p=',test_result$p,' w=',test_result$w,'\n',sep='')
    } else {
      cat('dep(',x,',',y,',',test_result$cset,',',test_result$jset,
            ',',weight,'). ', 
            '%p=',test_result$p,' w=',test_result$w,'\n',sep='')
    }
  } else { #new printing including the m set.
    if ( test_result$independent ) { #writing the independence
      cat('indep(',x,',',y,',',test_result$cset,',',
            test_result$jset,',',test_result$mset,',',
            weight,').',
            '%p=',test_result$p,' w=',test_result$w,'\n',sep='')
    } else {
      cat('dep(',x,',',y,',',test_result$cset,',',
            test_result$jset,',',test_result$mset,',',
            weight,').', 
            '%p=',test_result$p,' w=',test_result$w,'\n',sep='')
    }
  }  
}