randomGraph<-function(n,restrict=c('acyclic'),pedge=1/(n-1), topology="random") {
  #Function for generating random graph/linear gaussian model.
  # n  - number of observed variables
  # restrict - a vector with 'sufficient','acyclic' if these are wanted from the model.
  # pedge - the probability of an edge, this is good to be a decreasing function of n
  #         such that the density does not explode with increasing n
  #         (also note that for acyclic models the code , such that it is similar to )
  
  
  if ( !any(restrict=="sufficient")) {

    #Create a sufficient model for the causal part
    M<-randomGraph(n,union(restrict,"sufficient"),pedge=pedge/2)
    
    #draw a sufficient model for the covariances
    Me<-randomGraph(n,union(restrict,"sufficient"),pedge=pedge/2)
    
    #Then replace the covariance matrix with the passively observed cov. matrix
    #of the error model.
    A<-mpinv(diag(n)-Me$B)
    #adding herr some noise s.t. e are not deterministic functions
    #for example if we create noises e_x->e_y->e_z and the graph
    #does not have parents, x,y,z are is a deterministic function of es, 
    #and we will have x _||_z | y!
    M$Ce<-A%*%Me$Ce%*%t(A)+diag( abs(0.5+0.1*rnorm(n)) )
    
    #remember to update M$Ge
    M$Ge<-abs(M$Ce) > 1e-3    #replace also Ge which includes the true graph
    M$Ce[abs(M$Ce) < 1e-10] <-0
    diag(M$Ge)<-0
    
    return(M)
    
  }
  
  #sampling of the sufficient model
  M<-list(G=array(0,c(n,n)),Ge=array(0,c(n,n)),Gs=array(0,c(n,n)))
  if ( topology=='random') {
    #sample G either acyclic or possibly cyclic
    if ( any( restrict == 'acyclic') ) {
      order<-sample(1:n)
      pedge<-pedge*2 #multiply by two since fewer options
      M$G<-array(sample(c(0,1),n*n,replace=TRUE,prob=c(1-pedge,pedge)),
                  c(n,n))*lower.tri(M$G)
      diag(M$G)<-0    
      order<-sample(1:n)
      M$G[order,order]<-M$G #sample the order again
    } else {
      M$G<-array(sample(c(0,1),n*n,replace=TRUE,prob=c(1-pedge,pedge)),c(n,n))
      diag(M$G)<-0      
    }
    
    M$B<-M$G*matrix(runif(n*n,0.2,0.8),n,n)*matrix(sample(c(-1,1),n*n,replace=TRUE),n,n)  

  } else if (topology == 'example') {
    #y-structure
    M$G[3,1]<-M$G[3,2]<-M$G[4,3]<-1

    M$B<-M$G*matrix(runif(n*n,0.2,0.8),n,n)*matrix(sample(c(-1,1),n*n,replace=TRUE),n,n)
    
  }
  
  
  
  
  
  #sample a diagonal for covariance matrices, the insufficiency part is handled earlier
  M$Ce<-diag( abs(1+0.1*rnorm(n)) )
  
  M
}