plot5<-function(run=FALSE,howmany=300,pdf=FALSE) {
  #Supplement Figure 1.
  n<<-5
  
  exconf<<-'passive'
  howmany<<-howmany
  
  
  #RUN FIRST THE BEST THEN ADJUST THE SOLVING TIME FOR THE REST TO THE MAXIMUM
  #WE WANT ALL OF THESE RUNS TO GO THROUGH FOR THE BEST METHOD
  
  confs<<-list()
  #does it take too long to do the bayes tests...
  confs[[1]]<<-list(test="bayes",weight="log",p=0.5,solver="test",encode=NA,description='300 samples',schedule=n-2,N=300)
  confs[[2]]<<-list(test="bayes",weight="log",p=0.5,solver="test",encode=NA,description='500 samples',schedule=n-2,N=500)
  confs[[3]]<<-list(test="bayes",weight="log",p=0.5,solver="test",encode=NA,description='700 samples',schedule=n-2,N=700)
  confs[[4]]<<-list(test="bayes",weight="log",p=0.5,solver="test",encode=NA,description='900 samples',schedule=n-2,N=900)
  
  if ( run ) plot5.run()
  if ( !run ) plot5.plot(pdf)
}  

plot5.run<-function() {
  
  system('mkdir plot5')
  
  max_time<-0
  for ( i in 1:howmany ) {
    for ( k in 1:length(confs) ) {
      file=paste('plot5/run_',n,'_',k,'_',i,'.Rdata',sep='')
      if (file.exists(file)) next
      
      #always set seed to 0 in this test
      set.seed(i); #set seed to produce the same results
      cat('The seed set to:',i,'\n')
      L<-pipeline(n=n,exconf=exconf,
                  schedule=confs[[k]]$schedule,
                  test=confs[[k]]$test,p=confs[[k]]$p,
                  solver=confs[[k]]$solver,
                  encode=confs[[k]]$encode,weight=confs[[k]]$weight,
                  N=confs[[k]]$N,alpha=20)
      #save also the true model
      L$test_quality<-test_quality(M)
      #browser()
      L$M<-M
      save(L,file=file)
    }
  }
}


plot5.plot<-function(pdf) {  
  
  legend<-c()
  if ( pdf ) {
    pdf( 6,4,file='plot5.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.1) )
  }
  
  plot(-1,-1,xlim=c(0.03,0.97),ylim=c(0.03,0.97),type='b',pch=20,xlab='',ylab='')
  mtext("predicted probability of ind. (binned)",side=1,line=2)
  mtext("proportion of true ind.",side=2,line=2)
  for ( s in 0:10 ) {
    lines(0.1*(s)*c(1,1),c(-1,2))
    lines(c(-1,2),0.1*(s)*c(1,1))
  } 
  points(0.1*(0:9+0.5),0.1*(0:9+0.5),pch=5,col='orange' )
  image<-c()
  error<-c()
  objective<-c()
  
  str<-NULL
  for ( k in 1:length(confs) ) {
    bins_correct<-rep(0,10)
    bins_incorrect<-rep(0,10)    
    for ( i in 1:howmany ) {
      file<-paste('plot5/run_',n,'_',k,'_',i,'.Rdata',sep='')
      if (!file.exists(file)) next
      cat(file,'\n')
      load(file=file)

      for ( s in 0:9 ) {
        I<-L$test_quality$p > 0.1*s & L$test_quality$p <= 0.1*(s+1) 
        bins_correct[s+1]<-bins_correct[s+1]+sum(L$test_quality$true_independent[I] == 1)
        bins_incorrect[s+1]<-bins_incorrect[s+1]+sum(L$test_quality$true_independent[I] == 0)        
      }      
    }
    
    lines((0:9+0.5)/10,bins_correct/(bins_correct+bins_incorrect),ylim=c(0,1),xlim=c(0,1),type='p',col=k,pch=20)
    lines((0:9+0.5)/10,(bins_correct+bins_incorrect)/sum((bins_correct+bins_incorrect)),type='p',col=k,pch=1)
    
  }
  
  legend('topleft', legend=c('300 samples','500 samples','700 samples','900 samples','theoretical optimum'),col=c(1:4,'orange'),pch=c(rep(20,4),5),bg='white')

  if ( pdf ) {
    dev.off()
    cat('See the pdf!\n')
  }
  
  if (!pdf) browser()
  
}