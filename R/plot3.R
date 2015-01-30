plot3<-function(run=FALSE,howmany=100,pdf=FALSE) {
  #Figure 6 in the main paper.
  n<<-6
  N<<-500
  exconf<<-'passive'
  howmany<<-howmany
  
  schedule<<-n-2
  
  #These are run with the roughly "optimal" p parameters. 
  #This affects the running time greatly: if p is put to zero, all tests 
  #return independence and the empty graph can be found instantly.
  #
  
  confs<<-list()
  confs[[1]]<<-list(test="bayes",weight="log",p=0.1,solver="clingo",encode="old_wmaxsat.pl",description='log-weights (HHEJ 2013)' ,col=6)  
  confs[[2]]<<-list(test="bayes",weight="log",p=0.1,solver="clingo",encode="new_wmaxsat.pl",description='log-weights (new enc.)',col=3)
  confs[[3]]<<-list(test="classic",weight="constant",p=0.05,solver="clingo",encode="old_wmaxsat.pl",description='constant weights (HHEJ 2013)' ,col=5) 
  confs[[4]]<<-list(test="classic",weight="constant",p=0.05,solver="clingo",encode="new_wmaxsat.pl",description='constant weights (new enc.)',col='black') 
  confs[[5]]<<-list(test="classic",weight="constant",p=0.005,solver="clingo",encode="old_maxindep.pl",description='hard deps (HHEJ 2013)',col=4)  
  confs[[6]]<<-list(test="classic",weight="constant",p=0.005,solver="clingo",encode="new_maxindep.pl",description='hard deps (new enc.)',col='red')    
  
  if ( run ) plot3.run()
  if ( !run ) plot3.plot(pdf)
}  

plot3.run<-function() {
  
  system('mkdir plot3')
  
  for ( i in 1:howmany ) {
    for ( k in 1:length(confs) ) {
      file=paste('plot3/run_',n,'_',k,'_',i,'.Rdata',sep='')
      if (file.exists(file)) next
      
      set.seed(i); #set seed to produce the same results
      cat('The seed set to:',i,'\n')
      L<-pipeline(n=n,exconf=exconf,schedule=schedule,
                  test=confs[[k]]$test,p=confs[[k]]$p,
                  solver=confs[[k]]$solver,
                  encode=confs[[k]]$encode,weight=confs[[k]]$weight,N=N)
      #save also the true model
      L$M<-M
      save(L,file=file)
    }
  }
}


plot3.plot<-function(pdf) {  
  
  legend<-c()
  if ( pdf ) {
    pdf( 5,4,file='plot3.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.1) )
  }
  cols<-c()
  
  ks<-c(6,4,2,5,3,1)
  for ( k in ks ) {
    running_times<-c()
    solving_times<-c()
    encoding_times<-c()
    for ( i in 1:howmany ) {
      file<-paste('plot3/run_',n,'_',k,'_',i,'.Rdata',sep='')
      if (!file.exists(file)) next
      load(file=file)
      running_times<-c(running_times,L$solving_time+L$testing_time+L$encoding_time)
      solving_times<-c(solving_times,L$solving_time)
      encoding_times<-c(encoding_times,L$encoding_time)
    }
    if ( length(running_times) == 0 ) next
    
    #plot only solving times
    #(This is partly because the bayes test is not implemented optimally and it takes
    #more time that would actually be needed)
    running_times<-solving_times
    
    cat(confs[[k]]$description,
        sum(!is.infinite(running_times)),'/',length(running_times),
        'max=',max(running_times[!is.infinite(running_times)]),'\n')
    
    max<-100 #this is the largest time that is still plotted
    
    running_times[is.infinite(running_times)]<-max+10
    
    if ( k == ks[1] ) {
      #note that here always sorting the running times
      plot(sort(running_times),xlim=c(1,howmany),
           #main=paste('wmaxsat',n,'vars',exconf),
           ylim=c(0, max),#quantile(running_times,0.75)),
           type='l',col=confs[[k]]$col,xlab='',
           ylab="",pch=20)
      mtext("instances (sorted for each line)",side=1,line=2)
      mtext("solving time per instance (s)",side=2,line=2)
      points(sort(running_times),type='p',col=confs[[k]]$col,pch=20)
      
    } else {
      lines(sort(running_times),type='l',col=confs[[k]]$col,pch=20)
      points(sort(running_times),type='p',col=confs[[k]]$col,pch=20)
      
    }
    legend<-c(legend,confs[[k]]$description)
    cols<-c(cols,confs[[k]]$col)
  }  
  legend('topleft',legend=legend, col=cols,pch=20)
  
  if ( pdf ) dev.off()

}