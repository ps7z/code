learn.pcalg<-function(D,algo='pcalg-pc',p_threshold=0.05,test) { #,max_cset=5
  #Function for running the learning tasks for pc-derivatives.
  #D    data set from createDataSet.
  #algo "pcalg-pc","pcalg-cpc","pcalg-fci","pcalg-cfci"
  #p_threshold - p-value threshold to use
  #test - either "classic" or "oracle"
  
  
  #this can be whatever  
  write_data<-list()
  write_data$weight<-"constant"
  write_data$mode<-NA
  
  #must run these tests
  test_data<-list()
  tic()
  
  append<-FALSE
  jindex<-0
  schedule<- (n-2)
  for ( data in D ) {
    jindex<-jindex+1
    J<-which( data$e == 1  ) 
    #making sure the right J is used for printing
    write_data$jset<-bin.to.dec(rev(1*(data$e==1)))
    write_data$J<-J<-which(data$e==1)
    
    #putting in the test data as it should be
    #test_data$Cx<-diag(global_n)
    #if (is.null(data$N) ) data$N<-10000
    #test_data$N<-data$N
    #test_data$p_threshold<-p_threshold
    #test_function<-test.classic
    
    test_data$M<-M #should take out the Js here  
    test_data$N<-Inf
    test_function<-test.oracle
    test_data$M$G[J,]<-0 #making sure 
    test_data$M$Ge[J,]<-0
    test_data$M$Ge[,J]<-0    
    
    conduct(test_function,test_data,write_constraint,write_data,maxcset=schedule, append =append);    
  
    cat('\t\tDone for J={',paste(J,collapse=','),'} jset=',write_data$jset,' - ',jindex,'/',length(D),'\n',sep='')
    append<-TRUE
  }
  
  test_time <- toc()
  
  cat('\tDone! Took=',test_time,'seconds.\n')
  
  if ( test == "classic") {
    indepTest <- gaussCItest
    suffStat <- list(C = cor(D[[1]]$data), n = nrow(D[[1]]$data))
  } else if ( test == "oracle" ) {
    #make a function as an oracle
    indepTest<-pcalgOracle #this is implemented here
    suffStat <- list(M = M)
  }
  tic()
  
  

  if ( algo=="pcalg-pc") {
    
    pc.fit <- pc(suffStat, indepTest, p=global_n, alpha=p_threshold, verbose = TRUE)
    
  } else if ( algo== "pcalg-cpc" ) {
    #The modification return null whenever there are these unfaithful triples
    #another option would be to somehow average over the models cpc thinks
    #suitable. But this would be too much of a change, and then one might
    #also consider other models for ASP based algorithms as well as score
    #based stuff.
    pc.fit <- pc_mod(suffStat, indepTest, p=global_n, alpha=p_threshold,
                     verbose = TRUE,conservative=TRUE,u2pd="relaxed") 

    if ( is.null(pc.fit) ) { #return an unfit run if there is a problem
      return( list(solving_time=Inf) )      
    }
    
  } else if ( algo=="pcalg-fci" ) {
    
    fci.fit <- fci_mod(suffStat, indepTest, p=global_n, alpha=p_threshold, verbose = TRUE)
    
  } else if ( algo == "pcalg-cfci" ) {
    #conservative fci version, similarly returns null if unfaithful triples arie
    fci.fit <- fci_mod(suffStat, indepTest, p=global_n, alpha=p_threshold, verbose = TRUE,
                       conservative=c(TRUE,TRUE),cons.rules=TRUE)
    if ( is.null(fci.fit) ) {
      return( list( solving_time=Inf ) )      
    }
    
  }
  
  if ( algo=="pcalg-fci" || algo == "pcalg-cfci") {
    G<-fci.fit@amat
    L<-list(G=array(0,c(global_n,global_n)),
            Ge=array(0,c(global_n,global_n)),
            Gs=array(0,c(global_n,global_n)),
            Gcircles=array(0,c(global_n,global_n)))
    #now must turn G into G, Ge, Gs, Gcircles matrix
    #Object of class "matrix": The the estimated graph, represented by its adjacency matrix.
    #The edge marks are encoded by numbers: 0 = no edge, 1 = circle, 2 = arrowhead, 3 = tail. If
    #amat[i,j] = 1 and amat[j,i] = 2, this represents the edge i <-o j.
    #note that the rows and cols in the two notations are switched
    
    while (any(G!=0) ) {
      vars<-which(G!=0,arr.ind=TRUE)[1,]
      from<-vars[1];to<-vars[2];

      if (  G[to,from] == 2 &&  G[from,to] == 2  ) {#type == '<->' ) {
        
        L$Ge[to,from]<-L$Ge[from,to]<-1
        
      } else if (  G[to,from] == 3 &&  G[from,to] == 3  ) {#if ( type == '---' ) {
        
        L$Gs[to,from]<-L$Gs[from,to]<-1  
        stop('undirected edge in fci output')
        
      } else if (  G[to,from] == 1 &&  G[from,to] == 2  ) {#if ( type == 'o->') {
        
        #head-augmentation p. 106 Jiji Zhangs thesis
        L$G[to,from]<-1
        
      } else if (  G[to,from] == 2 &&  G[from,to] == 1  ) {#else if ( type == '<-o') {
        
        #head-augmentation p. 106 Jiji Zhangs thesis
        L$G[from,to]<-1
        
      }  else if (  G[to,from] == 3 &&  G[from,to] == 2  ) {#else if ( type == '-->') {
        
        L$G[to,from]<-1
        
      }  else if (  G[to,from] == 2 &&  G[from,to] == 3  ) {#else if ( type == '<--') 
      
        L$G[from,to]<-1
        
      } else if (  G[to,from] == 1 &&  G[from,to] == 3  ) {#if ( type == 'o--') {
        
        #head-augmentation p. 106 Jiji Zhangs thesis      
        L$Gs[from,to]<-1
        
      } else if (  G[to,from] == 3 &&  G[from,to] == 1  ) {#else if ( type == '--o') {
        
        #head-augmentation p. 106 Jiji Zhangs thesis
        L$G[to,from]<-1
        
      } else if (  G[to,from] == 1 &&  G[from,to] == 1  ) {#if ( type == 'o-o') {
        
        #leave as circles, will be oriented later!
        L$Gcircles[from,to]<-L$Gcircles[to,from]<-1
        
      }
      
      G[from,to]<-G[to,from]<-0
      
    }
    
    #still orient the circles such that the result is a MAG
    L<-pag22mag(L)
    
  }
  
  if (algo=="pcalg-pc" || algo== "pcalg-cpc" ) {
   G<-attr(pc.fit,'graph')

   E<-edges(G)
   L<-list(G=array(0,c(global_n,global_n)),
           Ge=array(0,c(global_n,global_n)),
           Gs=array(0,c(global_n,global_n)))
   
   for ( i in 1:length(E) ) {
     if ( length(E[[i]]) == 0 ) next
     L$G[as.numeric(E[[i]]),i]<-1
   }
   #browser()
   #now must note that undirected edges are plotted as <->
   L$Gs<-1*( L$G == 1 & t(L$G) == 1 )
   L$G<-L$G-L$Gs
   
   #turn the pdag into a dag
   L<-pattern2dag(L)
   
  }
  cat('Done...\n')
  
  L$solving_time <- toc()    
  
  L$objective <- NA
  L
}
