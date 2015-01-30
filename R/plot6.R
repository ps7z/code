plot6<-function(run=FALSE,howmany=100,pdf=FALSE) {
  #Figure 2 in the supplement.
  
  #some runs were done with these, not reported anywhere.
  #n<<-8
  #N<<-1000
  
  n<<-7
  N<<-500
  
  exconf<<-'passive'
  howmany<<-howmany
  
  #RUN FIRST THE BEST THEN ADJUST THE SOLVING TIME FOR THE REST TO THE MAXIMUM
  #WE WANT ALL OF THESE RUNS TO GO THROUGH FOR THE BEST METHOD
  
  confs<<-list()
  #does it take too long to do the bayes tests...
  confs[[1]]<<-list(test="bayes",weight="log",p=0.1,solver="clingo",encode="new_wmaxsat.pl",description='log-weights (new enc.) max cset = 3',schedule=3)
  confs[[2]]<<-list(test="bayes",weight="log",p=0.1,solver="clingo",encode="new_wmaxsat.pl",description='log-weights (new enc.) max cset = 5',schedule=n-2)
  
  if ( run ) plot6.run()
  if ( !run ) plot6.plot(pdf)
}  

plot6.run<-function() {
  
  system('mkdir plot6')
  
  max_time<-0
  for ( i in 1:howmany ) {
    for ( k in 1:length(confs) ) {
      file=paste('plot6/run_',n,'_',k,'_',i,'.Rdata',sep='')

      if (file.exists(file)) {
        load(file);
        if (!is.infinite(L$solving_time) ) next;
      }
      
      set.seed(i); #set seed to produce the same results
      cat('The seed set to:',i,'\n')
      L<-pipeline(n=n,exconf=exconf,schedule=confs[[k]]$schedule,
                  test=confs[[k]]$test,p=confs[[k]]$p,solver=confs[[k]]$solver,
                  encode=confs[[k]]$encode,weight=confs[[k]]$weight,alpha=1.5,N=N)
      #save also the true model
      L$M<-M
      save(L,file=file)
    }
  }
}


plot6.plot<-function(pdf) {  

  legend<-c()
  if ( pdf ) {
    pdf( 6,4,file='plot6.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.1) )
  }
  
  
  for ( k in 1:length(confs) ) {
    running_times<-c()
    solving_times<-c()
    encoding_times<-c()
    for ( i in 1:howmany ) {
      file<-paste('plot6/run_',n,'_',k,'_',i,'.Rdata',sep='')
      if (!file.exists(file)) next
      load(file=file)
      
      running_times<-c(running_times,L$solving_time+L$testing_time+L$encoding_time)
      solving_times<-c(solving_times,L$solving_time)
      encoding_times<-c(encoding_times,L$encoding_time)
      
    }
    if ( length(running_times) == 0 ) next
    
    #printing solving times only, since the tests are not implement 
    #exactly optimally yet
    running_times<-solving_times
    
    
    cat(confs[[k]]$description,
        sum(!is.infinite(running_times)),'/',length(running_times),
        'max=',max(running_times[!is.infinite(running_times)]),'\n')
    max<-1200
    
    print(running_times)
    running_times[is.infinite(running_times)]<-max*10
    running_times<-log10(running_times)
    max<-log10(max)
    
    if ( k == 1 ) {
      plot(sort(running_times),xlim=c(1,howmany),
           #main=paste('wmaxsat',n,'vars',exconf),
           ylim=c(0, max),#quantile(running_times,0.75)),
           type='l',col=k,xlab='',
           ylab="",pch=20,axes=FALSE)
      mtext("instances (sorted for each line)",side=1,line=2)
      mtext("solving time per instance (s)",side=2,line=2)
      points(sort(running_times),type='p',col=k,pch=20)
      
    } else {
      lines(sort(running_times),type='l',col=k,pch=20)
      points(sort(running_times),type='p',col=k,pch=20)
      
    }
    legend<-c(legend,confs[[k]]$description)
  }  
  legend('bottomright',legend=legend, col=1:length(confs),pch=20)

  box()
  axis(1,at=seq(0,100,by=20),labels=TRUE)
  axis(2,at=c(0,1,2,3),labels=c('1','10','100','1000'))
  
  if ( pdf ) {
    dev.off()
    cat('See the pdf!\n')
  }
  
  if (!pdf) browser()
}