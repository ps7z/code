learn<-function(   D, #data
                   test="classic",schedule=3,
                   weight="constant",encode="plain",
                   p=0.05,
                   alpha=1.5,
                   clingoconf="--configuration=crafty --time-limit=25000 --verbose=2",
                   verbose=12) {
  #Pipeline for running the asp inference
  #options:
  #Independence test properties:
  ##############################
  #test     -type of test used to generate data, outputs a value between 0 and 1?
  #         -"classic" classic correlation test (p-value = 0.05)
  #         -"oracle" independence facts determined by Patrik's oracle
  #         -"BIC" BIC-based calculation
  #         -"bayes" integrating over the parameters introduced in the paper
  #schedule -which independence 
  #         -if a number T it means all tests up to intervention tests size T
  #           (can be Inf, default = 3)
  #weight   -how to determine the weights from the dependencies:
  #         -"log" take log of the probability
  #         -"constant" the most likely option gets weight 1
  #p        -for bayes and BIC tests the apriori probability of 
  #         -for classic test using algorithms the p-value threshold
  #         -for BIC-based score based learning the prior parameter
  #alpha    -for bayes test the eq. sample size
  ###############################
  #encode   - which encoding to use, gives the file in the ASP/ directory
  #clingoconf  - a string which defines additional parameters to clingo
  #Printing options:
  #verbose  -0 to not print anything, 1 to print stuff for now
  if ( is.na(p) ) p<-0.5
  
  #this is a global list holding the indeps.
  global_indeps<<-list()
  
  write_data<-list()
  write_data$weight<-weight
  write_data$mode<-encode
  
  if (verbose) cat("\tConducting independence tests: schedule/cmax=",schedule,
                   ", test=",test,".\n",sep='')
  tic()
  
  test_data<-list()
  
  append<-FALSE
  jindex<-0
  for ( data in D ) {
    jindex<-jindex+1
    #making sure the right J is used for printing
    write_data$jset<-bin.to.dec(rev(1*(data$e==1)))
    write_data$J<-J<-which(data$e==1)
    
    #putting in the test data as it should be
    if ( test == "classic") {
      test_data$Cx<-cov(data$data)
      test_data$N<-data$N
      test_data$p_threshold<-p
      test_function<-test.classic 
    } else if ( test == 'oracle') {
      test_data$M<-data$M #should take out the Js here  
      test_data$N<-Inf
      test_function<-test.oracle
      test_data$M$G[J,]<-0 #making sure 
      test_data$M$Ge[J,]<-0
      test_data$M$Ge[,J]<-0        
    } else if (test == "BIC") {
      test_data$X<-data$data
      test_data$p_threshold<-p
      test_function<-test.BIC      
    } else if (test == "bayes") {
      test_data$X<-data$data
      test_data$p_threshold<-p #prior probability of ind.
      test_function<-test.bayes
      test_data$alpha<-alpha #eq. sample size for the prior
    }
    
    conduct(test_function,test_data,write_constraint,write_data,maxcset=schedule,append=append);    

    cat('\t\tDone for J={',paste(J,collapse=','),'} jset=',write_data$jset,' - ',jindex,'/',length(D),'\n',sep='')
    append<-TRUE
  }
  
  test_time <- toc()
  
  cat('\tDone! Took=',test_time,'seconds.\n')
  #browser()
  
  if ( is.na(encode) ) { #this was the way to get test quality
                                     #so coould take out test quality all together...
    L<-list(solving_time=0,testing_time=test_time,encoding_time=0,objective=NA)
    return(L)
  }
  ##############################################################################
  
  cat('\tConducting encoding operations...\n')  
  tic()    
  asp_program<-paste('./../ASP/',encode,sep='')    
  
  if ( grepl('new_',encode) ) { 
    cat('\t\tWriting asp constants (new encoding)...\n')
    #if using the new encoding
    writeAspSets.old(n,file="./../tmp/pipeline.pre.asp")
    
    #building the encoding DAG
    build_tree(file="./../tmp/pipeline.pre.asp" )
  } else {
    cat('\t\tWriting asp constants (old encoding)...\n')
#    writeAspSets(n,file="./../tmp/pipeline.pre.asp")
    cat('\t\tDone.\n')
  }
  
  encoding_time<-toc()
  if (verbose) cat('\tDone! Took=',encoding_time,'seconds.\n')
  
  tic()
  
  if (verbose) cat("\t\tRunning Clingo.\n",sep='')
  system(paste('rm','./../tmp/pipeline.ind.clingo'))
  
  system(paste("./../ASP/clingo430",
               clingoconf,
               './../tmp/mypipeline.pre.asp','./../tmp/mypipeline.ind',asp_program,
               '| tee ./../tmp/pipeline.ind.clingo') )
  
  sol_file<-"./../tmp/pipeline.ind.clingo"    
  
  solving_time<-toc()
  #system("voieee")
  if (verbose) cat('\tDone! Took=',solving_time,'seconds.\n')
  #############################################################################
  #############################################################################
  if (verbose) cat('\tParsing the solution:\n')
  
  L<-parse_solution(solver='clingo',encode,sol_file)
  #browser()
  if (all(is.na(L)) ) {
    L<-list()
    solving_time<-Inf
  }
  
  L$testing_time<-test_time
  L$encoding_time<-encoding_time
  L$solving_time<-solving_time
  
  L$test<-test
  L$encode<-encode
  L$schedule<-schedule
  L$weight<-weight
  
  cat('\tDone.\n')
  L
}
