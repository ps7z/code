objective<-function(M,global_indeps,asp=TRUE,weight="log",verbose=TRUE) {
  #Calculates the objective function value, defined by the weight-function.
  #M      - model for which the cost is calculated
  #global_indeps - list of the independence relations
  #asp   - if asp is true the each weights is multiplied by 1000 and rounded, otherwise not.
  #weight - "log" or "constant"
  #verbose - if >= 1, the relations with suffered cost will be printed
  
  #For a greedy procedure this could be done much faster...
  
  value<-0
  i<-0 #index since looping through global indeps
  for ( indep in global_indeps) { 
    i<-i+1
    #Calculate the relation indep in the model M
    independent<-!directed_reachable(indep$vars[1],indep$vars[2],indep$C,indep$J,M)
    
    if ( independent != indep$independent) { #if different from the most likely option, suffer some loss
      if ( !asp && weight == "log") {
        value<-value + indep$w          
      } else if ( weight == "log") {
        value<-value + round(1000*indep$w)  
        indep$w<-round(1000*indep$w) 
      } else if ( weight == "binary" || weight=="bin" ||weight == "none" || weight=='constant' ) {
        value<-value + 1        
      }
      data_mark <- '_||_'
      if ( !indep$independent ) data_mark <- '_N_'
      learned_mark <- '_||_'
      if (!independent ) learned_mark <- '_N_'
      
      if ( verbose) cat(i,':',indep$vars[1],'?',indep$vars[2],'|',paste(indep$C,collapse=','),
                        '||',paste(indep$J,collapse=','),
                        'data:',data_mark,'learned:',learned_mark,'suffered loss',indep$w,'\n')
    }
    #cat(value,'\n')
  }
  value
}