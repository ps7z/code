test<-function(algo='log-weights',n=6) {
  #Function for running simple tests. These tests are run in the most
  #general model space allowing for cycles and latents(=bidirected arcs). Endless
  #number of parameter combinations exist for running the inference, see the plots
  #for some of the most relevant combinations for calling the pipeline function.
  #
  #algo     - defines the algorithm run.
  #     "log-weights" or 1 - the log weighting scheme [section 4.3 in the paper]
  #                          the fastest and most accurate method
  #     "hard-deps" or 2  - putting in deps as hard constraints and max.
  #                         the number of independiencies [section 4.1 in the paper]
  #     "constant-weights" or 3 - weight of 1 for both indeps and desp
  #                             [section 4.2 in the paper]
  
  
  
  #using a single passive observational data set here
  exconf<-'passive'
  #and doing all tests
  schedule<<-n-2
  N<-500  #total number of samples

  #what are the different 
  if ( algo=='log-weights' || algo==1 ) {
    test<-"bayes"
    weight<-"log"
    p<-0.4 #roughly optimal for 500 samples of passively observed data (paper ROCs)
    alpha<-20   
    #another roughly optimal set of parameters
    #p<-0.08
    #alpha<-1.5
    solver<-"clingo"
    encode<-"new_wmaxsat.pl"
    restrict<-c()
  } else if ( algo == 'hard-deps' || algo==2) {
    test<-"classic"
    weight<-"constant"
    p<-0.001 #roughly optimal parameters for 500 samples
    solver<-"clingo"
    encode<-"new_maxindep.pl"
    restrict<-c()
  } else if ( algo == 'constant-weights' || algo==3) {
    test<-"classic"
    weight<-"constant"    
    p<-0.05 #quite optimal for constant weights
    encode<-"new_wmaxsat.pl"
    solver<-"clingo"
    restrict<-c()
  } else if ( algo == 'test only' || algo==4 ) {
    #note that for all different proposed tests (bayes,BIC,classic)
    #the test performances give out roughly the same ROC curve, due to
    #the fact that the inference is quite similar in the end. 
    #This does not mean that the probabilities are same, as ROC does not take 
    #into account the probabilities of the constraints.
    
    test<-"classic"
    weight<-"constant"
    solver<-"test"
    encode<-NA
    p<-0.05
    restrict<-c()
  }
    
  #run the inference
  L<-pipeline(n=n,exconf=exconf,schedule=schedule,
            test=test,p=p,solver=solver,
            encode=encode,weight=weight,N=N,restrict=restrict)
}