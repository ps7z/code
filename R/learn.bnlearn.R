learn.bnlearn<-function(D,p_threshold=1,test) {
  #Function for running the learning tasks for score-based learning.
  #This implements an exact search over the bnlearn package.
  #The function is rather a quick fix.
  #D    data set from createDataSet.
  #p_threshold - sparsity penalty for the BIC local score
  
  #this can be whatever  
  write_data<-list()
  write_data$weight<-"constant"
  write_data$mode<-NA
  
  test_data<-list()
  tic()
  
  #here running the tests, because these are needed for the comparison
  append<-FALSE
  jindex<-0
  schedule<-(n-2)
  for ( data in D ) {
    jindex<-jindex+1
    J<-which( data$e == 1  ) 
    #making sure the right J is used for printing
    
    test_data$M<-M #should take out the Js here  
    test_data$N<-Inf
    test_function<-test.oracle
    test_data$M$G[J,]<-0 #making sure 
    test_data$M$Ge[J,]<-0
    test_data$M$Ge[,J]<-0    
    
    conduct(test_function,test_data,write_constraint,write_data,maxcset=schedule, append =append);    
    
    cat('\t\tDone for J={',paste(J,collapse=','),'} jset=',write_data$jset,
        ' - ', jindex,'/',length(D),'\n',sep='')
    append<-TRUE
  }
  
  test_time <- toc()
  
  cat('\tDone! Took=',test_time,'seconds.\n')
  
  tic()
  
  df<-data.frame(D[[1]]$data)
  
  #exhaustive search, not optimally fast here since looping over orders
  #there does not seem to be a proper implementation of score-based learning
  #in the exact sense for R
  
  #empty graph
  #overall the best graph
  bestbestG<-array(0,c(n,n))
  bestbestscore<-(-Inf)
  ind<-0
  
  for ( corder in permn(global_n) ) { #loop through all causal orders
    ind<-ind+1
    #These are the best options for the order
    bestG<-array(0,c(n,n))
    bestscore<-bnlearn::score(as.bn( G2st(bestG) ),df,k=p_threshold*log(nrow(df))/2)
    for ( i in global_n:2 ) { #consider nodes from the end of the order to beginning
      G<-bestG #always start with the best here
      node<-corder[i]
        
      #cat('\tnode:',node,'\n')
        
      #find the possible parents for the node
      possible_parents<-corder[index(1,(i-1))]
        
      #print(possible_parents)
      #cat('\t\tpossible_parents:',possible_parents,'\n')
        
      bestscore<-(-Inf)
      for ( ipa in index(0,2^length(possible_parents)-1)) {#do not have consider 0
        G[node,possible_parents]<-dec.to.bin(ipa,length(possible_parents))
        score<-bnlearn::score(as.bn( G2st(G) ),df,k=p_threshold*log(nrow(df))/2)
        #cat(G[node,],'score=',score)
        if ( score > bestscore ) {
          bestG<-G
          bestscore<-score
          #cat('!')
        } 
        #cat('\n')
      }#parent configuration
    }#for i/node
    #now bestG is the best given this order, updating if it is the best given all orders
    if ( bestscore > bestbestscore) {
      bestbestG<-bestG
      bestbestscore<-bestscore
    }
    cat(ind,'bestscore:',bestscore,'bestbestscore:',bestbestscore,'\n')
  }#for order
  
  #make the L object from the best graph
  L<-list()
  L$G<-bestbestG
  L$Ge<-array(0,c(global_n,global_n))  

  L$solving_time<-toc()
  cat('Done...\n')

  L$objective <- NA
  L
}


