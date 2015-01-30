plot7<-function(run=FALSE,howmany=200,pdf=FALSE) {
  #Figure 5 right in the actual paper. 
  n<<-6
  N<<-500

  exconf<<-'passive'
  howmany<<-howmany
  
  schedule<<-n-2
  topology<<-"random"
  restrict<<-c()
  
  confs<<-list()

  confs[[1]]<<-list(test="classic",weight="constant",
                    p=c(0.000001,0.00001,0.0001,0.0005,0.001,
                        0.005,0.01,0.05,0.1,0.15,0.2),
                    solver="clingo",encode="new_maxindep.pl",
                    description='hard deps [sec. 4.1]',col=2,pch=20)  
  
  confs[[2]]<<-list(test="classic",weight="constant",
                    p=c(0.001,0.005,0.01,0.03,0.05,0.1,0.15,0.2,0.3),
                    solver="clingo",encode="new_wmaxsat.pl",
                    description='constant weights [sec. 4.2]',col=1,pch=20) 
  
  
  confs[[3]]<<-list(test="bayes",weight="log",
                    p=c(0.9999,0.999,0.99,0.9,0.7,0.5,0.4,0.3,0.2,0.15,
                        0.1,0.09,0.08,0.07,0.06,0.05,0.04,0.03,0.02,0.01),
                    alpha=1.5,
                    solver="clingo",encode="new_wmaxsat.pl",
                    description='log-weights [sec. 4.3]',col=3,pch=20) 
  
  
  confs[[4]]<<-list(test="classic",weight="none",
                    p=c(0.0001,0.001,0.01,0.05,0.1,0.15,0.2,0.3,0.4),
                    solver="test",encode=NA,description='test only',col='blue',pch=5)
    
  
  if ( run ) plot7.run()
  if ( !run ) plot7.plot(pdf=pdf)
}  

plot7.run<-function() {
  
  system('mkdir plot7')
  
  for ( i in 1:howmany ) {
    for ( k in 1:5 ) {
      for ( p in confs[[k]]$p ) {
        file=paste('plot7/run_',n,'_',k,'_',i,'_',p,'.Rdata',sep='')
        if (file.exists(file)) next
        
        set.seed(i); #set seed to produce the same results
        cat('The seed set to:',i,'\n')
        L<-pipeline(n=n,topology=topology,exconf=exconf,schedule=schedule,
                    test=confs[[k]]$test,p=p,solver=confs[[k]]$solver,
                    alpha=confs[[k]]$alpha,
                    encode=confs[[k]]$encode,weight=confs[[k]]$weight,
                    N=N,restrict=restrict)
        #save also the true model
        L$M<-M
        save(L,file=file)
        #browser()
      }
    }
  }
}


plot7.plot<-function(pdf=FALSE) {  

  legend<-c()
  pchs<-c()
  cols<-c()

  if ( pdf ) {
    pdf( 5,4,file='plot7.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.1) )
  }
  pi<-0
  ks<-c(2,1,3,5) #4 is with alt. prior, out for now
  #ks<-3
  for ( k in ks ) {
    pi<-pi+1
    FPvec<-c()
    TPvec<-c()
    FNvec<-c()
    TNvec<-c()
    modelsvec<-c()
    
    for ( p in confs[[k]]$p ) {
      FP<-TP<-FN<-TN<-0
      models<-0
      for ( i in 1:howmany ) {
        file=paste('plot601/run_',n,'_',k,'_',i,'_',p,'.Rdata',sep='')
        if (!file.exists(file)) {
          cat(k,i,p,"NOT RUN YET!!!!\n")
          browser()
          break
        }
        load(file=file)

        FP<-FP+L$added
        FN<-FN+L$deleted
        TP<-TP+L$totaldeps-L$deleted
        TN<-TN+L$totalindeps-L$added
        models<-models+1
      }
      FPvec<-c(FPvec,FP)
      FNvec<-c(FNvec,FN)
      TPvec<-c(TPvec,TP)
      TNvec<-c(TNvec,TN)
      modelsvec<-c(modelsvec,models)
      cat('p=',p,TP,TN,FP,FN,'models:',models,'/',howmany,'OK\n')
    }
    TPRvec<-TPvec/(TPvec+FNvec)
    FPRvec<-FPvec/(FPvec+TNvec)
    Pvec<-TPvec/(TPvec+FPvec)
    cat(confs[[k]]$description,':', 
        sum(FNvec+TPvec)/length(confs[[k]]$p) ,'\n')
    print(rbind(confs[[k]]$p,FPRvec,TPRvec,modelsvec))
    
    
    if ( k == 1 ) {
      plot(c(0,FPRvec,1),c(0,TPRvec,1),xlim=c(0,0.3),
             ylim=c(0.75,0.92),
             type='l',col=confs[[k]]$col,
             xlab='',ylab="",pch=confs[[k]]$pch)
      mtext("FPR",side=1,line=2)
      mtext("TPR",side=2,line=2)
        
      points(FPRvec,TPRvec,col=confs[[k]]$col,pch=confs[[k]]$pch)
      lines(c(0,1),c(0,1))
    } else {
      lines(c(0,FPRvec,1),c(0,TPRvec,1),type='l',col=confs[[k]]$col,pch=20)
      points(FPRvec,TPRvec,col=confs[[k]]$col,pch=confs[[k]]$pch)#+k%%3)
    }
    
    if (!pdf) browser()

    legend<-c(legend,confs[[k]]$description)
    pchs<-c(pchs,confs[[k]]$pch)
    cols<-c(cols,confs[[k]]$col)
    
  }  
  
  
  legend('bottomright', legend=legend, col=cols, pch=pchs)

  #should have the old encoding with minor mods in the SAT world?
  #may that is just it, for SAT the old beats the new and all.
  if ( pdf ) {
    dev.off()
    cat('See the pdf!\n')
  }
  
  if (!pdf) browser()
}