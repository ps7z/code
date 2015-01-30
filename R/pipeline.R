pipeline<-function(n=4, topology="random", #model properties
                   exconf="passive",N=1000, #sampling properties
                   test="bayes",schedule=n-2, #testing properties
                   weight="log",encode="new_wmaxsat.pl", #how to encode them
                   solver="clingo", #which solver to use
                   p=0.1, alpha=1.5,   ##eq. sample size for the bayes test
                   verbose=1, #how much information to print
                   model=NULL, #input model if given
                   clingoconf="--configuration=crafty --time-limit=25000 --quiet=1,0",
                   restrict = c() #what restrictions apply to model space, note that encode file 
                                  #tells which restrictions are forced by the learned model space
                   ) {
  #Pipeline for running the algorithm inference. This first creates model and data, then runs
  #the requested inference and finally assesses and prints out the quality of the inference.
  #Use "set.seed()" before this function to compare the performance of several algorithms.
  #
  # Model properties:
  ###################
  # n  - number of observed variables (default=3)
  #
  #Data properties:
  #################
  #exconf   - experiment configuration, "passive" (default) or "single" or "random"
  # N       -total number of data samples (divided equally to all experiments) (default=1000)
  #
  #Independence test properties:
  ##############################
  #test     -type of test used to generate data, outputs a value between 0 and 1?
  #         -"classic" classic correlation test (p-value = 0.05)
  #         -"oracle" independence facts determined by Patrik's oracle
  #         -"bayes" integrating over the parameters
  #         -"BIC" (default) BIC-based approximation of the bayes test, faster and almost as good
  #         -the prior parameters are a little different, only p as the prior prob. is needed.
  #schedule -independence test scheduling
  #         -maximum conditioning set size for the independence tests
  #         -if a number T it means all tests up to intervention tests size T
  #           (can be Inf, default n-2 so all possible conditioning test sizes)
  #weight   -how to determine the weights from the dependencies:
  #         -"log" take log of the probability
  #         -"constant" the most likely option (dep or indep) gets weight 1
  #         -"hard-deps" put dependencies as hard constraints
  #         -for competing algorithms, pc and so on, use any of the above.
  ###############################
  #encode   - which encoding to use, gives the file in the ASP/ directory
  #solver   -"clingo", or "pcalg-pc","pcalg-cpc","pcalg-fci","pcalg-cfci" or "bnlearn" for score based learning
  #p        -for bayes and BIC tests the apriori probability of 
  #         -for classic test using algorithms the p-value threshold
  #         -for BIC-based score based learning the prior parameter
  #alpha    -for bayes test the eq. sample size
  ###############################
  # clingoconf  - a string which defines additional parameters to clingo
  #Printing options:
  #verbose  -0 to not print anything, 1 to some printing
  
  #setting n globally
  global_n<<-n<<-n
  global_indeps<<-list()
  
  if (verbose) cat('1st stage: Generate data.\n')
  ##############################################################################
  if (verbose) cat("\tGenerating the model: n=",n,'.\n',sep='')
  
  if ( is.null(model) ) {
    M<<-randomGraph(n=n,restrict=restrict,topology=topology)
  } else { #if model is given use that
    M<<-model
  }
  
  if (verbose > 10) print(M)
  
  ##############################################################################
  if (verbose) cat("\tGenerating experiment configuration: exconf=",exconf,'.\n',sep='')
  
  E<-experimentConfiguration(n,exconf)
  
  if (verbose > 10) print(E)
  
  if ( test == "oracle" ) { 
    if (verbose) cat("\tSkipping sample data generation, since using oracle.\n",sep='')  
    D<-list()
    for ( i in 1:nrow(E)) { #the data consist of manipulated graphs where
                            #edge heads into the intervened variables are cut
      D[[i]]<-list(e=E[i,],M=M)
      J<-which( D[[i]]$e==1 )
      D[[i]]$M$G[J,]<-0
      D[[i]]$M$Ge[J,]<-0
      D[[i]]$M$Ge[,J]<-0
    }
  } else {
    if (verbose) cat("\tGenerating sample data: N=",N,'.\n',sep='')
    D<-createDataSet(M,E,N)
  }
  cat('1st stage: Done.\n')
  
  ##############################################################################
  if (verbose) cat('2nd stage: Run the learning algorithm.\n')  
  
  if ( any( solver== c("pcalg-pc","pcalg-cpc","pcalg-fci","pcalg-cfci")  )) {
    L<-learn.pcalg(D,algo=solver,p_threshold=p,test);
  } else if ( any( solver== c("bnlearn")  ) ) {
    L<-learn.bnlearn(D,p_threshold=p,test);    
  } else {   #clingo or test
    L<-learn( D, #data
            test=test,schedule=schedule,
            weight=weight,encode=encode,
            p=p, alpha=alpha, clingoconf=clingoconf,verbose=1)
  }
  
    
  cat('2nd stage: Done.\n')  
  #########################################################################
  #cat('Learned Model:\n')
  #print(L)
  #browser()
  if ( is.infinite(L$solving_time) ) {
    return(L)
  }
  
  cat('3rd Stage: Evaluation.\n')
  cat('\tObjective of the learned model claimed by solver=', L$objective,'\n')

  if (  solver == "clingo" ) {
    cat('\tObjective of the learned model (recalculated)= ')    
    objective_L<-objective(L, global_indeps, asp=(solver=='clingo'), weight=weight, verbose=0 )
    cat(objective_L,'\n')

    if ( abs(objective_L - L$objective) > 1e-3 &&
      round(1000*objective_L) != L$objective &&
      round(objective_L) != round(1000*L$objective) ) {
        cat('\tObjective check negative.\n')  
    } else {
      cat('\tObjective check OK.\n')        
    }
  } else {
    cat('\tObjective check SKIPPED.\n')  
  }
  cat('\tObjective of the true model= ')
  objective_M<-objective(M,global_indeps,asp=(solver=='clingo'),
                           weight=weight,verbose=0 )
  cat(objective_M,'\n')
  L$true_objective<-objective_M

  cat('\tLearned model added= ')
  cmp<-compare(L=L,M=M,verbose=0,passive=FALSE)

  cat(cmp$added,'and deleted=',cmp$deleted,'dependencies.\n')
    
  L$added<-cmp$added
  L$deleted<-cmp$deleted 
  L$totaldeps<-cmp$totaldeps
  L$totalindeps<-cmp$totalindeps
  
  cat('3rd Stage: Done.\n')
  
  ############################################################################
  #Add various input info to L
  L$n<-n
  L$exconf<-exconf
  L$N<-N
  
  cat('n & edges & experiments &',
      'schedule & test & weight & p &',
      'encode & ',      
      'test(sec) & encode(sec) & solve(sec) &',
      'add deps & del deps ',
      '\\\\\n')
  cat(n,'&',sum(M$G)+sum(M$Ge)/2,'&',exconf,'&',
      schedule,'&',test,'&',weight,'&',p,'&',
      encode,'&',
      L$testing_time,'&',L$encoding_time,'&',L$solving_time,
      '&',L$added,'&',L$deleted, ' \\\\\n')

  invisible(L)
}
