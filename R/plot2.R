plot2<-function(run=FALSE,howmany=200,pdf=FALSE) {
  #Figure 5 center in the main paper.
  
  #Settings for th run
  n<<-6
  N<<-500
  exconf<<-'passive'
  howmany<<-howmany
  schedule<<-n-2
  restrict<<-c('acyclic')
  
  confs<<-list()
  confs[[1]]<<-list(test="classic",weight="constant",
                    p=c(0.000001,0.00001,0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.15,0.2),
                    #p=c(0.0005,0.001,0.005),
                    solver="clingo",encode="new_maxindep_acyclic.pl",
                    description='hard deps [sec. 4.1]',col=2,pch=20)
  
  confs[[2]]<<-list(test="classic",weight="constant",
                    p=c(0.005,0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.35),
                    #p=c(0.01,0.05,0.1),
                    solver="clingo",encode="new_wmaxsat_acyclic.pl",
                    description='constant weights [sec. 4.2]',col=1,pch=20)
  
  
  confs[[3]]<<-list(test="bayes",weight="log",
                    p=c(0.9999,0.999,0.99,0.9,0.7,0.5,0.4,0.3,0.2,0.1,
                        0.09,0.08,0.07,0.06,0.05,0.04,0.03,0.02,
                        0.01,0.001,0.0001),
                    #p=c(0.2,0.1,0.05),  
                    solver="clingo",encode="new_wmaxsat_acyclic.pl",
                    description='log-weights [sec 4.3]',col=3,pch=20)
    
  confs[[4]]<<-list(test="classic",weight="constant",
                    p=c(0.0001,0.001,0.01,0.05,0.1,0.15,0.2,0.3,0.4),
                    #p=c(0.01,0.05,0.1),
                    solver="pcalg-fci",encode=NA,description='FCI (pcalg)',col=5,pch=1)
  
  confs[[5]]<<-list(test="classic",weight="constant",
                    p=c(0.0001,0.001,0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.45,0.5,0.6,0.7,0.8),
                    #p=c(0.01,0.05,0.1),
                    solver="pcalg-cfci",encode=NA,description='cFCI (pcalg) (subset)',col=6,pch=1)
  
  confs[[6]]<<-list(test="classic",weight="constant",
                    p=c(0.0001,0.001,0.01,0.05,0.1,0.15,0.2,0.3,0.4),
                    #p=c(0.01,0.05,0.1),
                    solver="test",encode=NA,description='test only',col=4,pch=5)
  
  
  if ( run ) plot2.run()
  if ( !run ) plot2.plot(pdf=pdf)
}  

plot2.run<-function() {
  
  system('mkdir plot2')
  
  for ( i in 1:howmany ) {
    for ( k in 1:length(confs) ) {
	#if ( any( k == c(1,2) ) ) next
      for ( p in confs[[k]]$p ) {
        file=paste('plot2/run_',n,'_',k,'_',i,'_',p,'.Rdata',sep='')
        if (file.exists(file)) next #skip if done
        
        set.seed(i); #set seed to produce the same results
        cat('The seed set to:',i,'\n')
        L<-pipeline(n=n,exconf=exconf,schedule=schedule,
                    test=confs[[k]]$test,p=p,solver=confs[[k]]$solver,
                    encode=confs[[k]]$encode,weight=confs[[k]]$weight,N=N,alpha=1.5,
                    restrict=restrict)
        #save also the true model
        L$M<-M
        save(L,file=file)
      }
    }
  }
}


plot2.plot<-function( pdf=FALSE ) {  
  
  legend<-c()
  pchs<-c()
  cols<-c()
  
  if ( pdf ) {
    pdf( 5,4,file='plot2.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.1) )
  }
  
  for ( k in 1:6  ) {
    
    FPvec<-c()
    TPvec<-c()
    FNvec<-c()
    TNvec<-c()
    modelsvec<-c()
    
    for ( p in confs[[k]]$p ) {
      FP<-TP<-FN<-TN<-0
      models<-0
      for ( i in 1:howmany ) {
        file=paste('plot2/run_',n,'_',k,'_',i,'_',p,'.Rdata',sep='')
        if (!file.exists(file)) {
          cat(i,"NOT RUN YET!!!!\n")
          #browser()
          next
        }
        
        load(file=file)
        if ( is.infinite(L$solving_time)) {
          cat('RUN skipped!!!\n')
          next
        }
        
        cat(L$added,L$deleted,L$totaldeps,L$totalindeps,'\n')
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
    print(rbind(confs[[k]]$p,FPRvec,TPRvec,Pvec,modelsvec))
    
    
    lty<-"solid"
    if( k==5) lty<-"dashed"
    
    if ( k == 1 ) {
      plot(c(0,FPRvec,1),c(0,TPRvec,1),xlim=c(0,0.3),
            ylim=c(0.75,0.92),
            type='l',col=confs[[k]]$col,xlab='',ylab="",pch=20)
      mtext("FPR",side=1,line=2)
      mtext("TPR",side=2,line=2)
      points(FPRvec,TPRvec,col=confs[[k]]$col,pch=confs[[k]]$pch)
      lines(c(0,1),c(0,1))
    } else {
      lines(c(0,FPRvec,1),c(0,TPRvec,1),type='l',lty=lty,col=confs[[k]]$col)
      points(FPRvec,TPRvec,col=confs[[k]]$col,pch=confs[[k]]$pch)
    }

    if ( !pdf ) browser()
    legend<-c(legend,confs[[k]]$description)
    pchs<-c(pchs,confs[[k]]$pch)
    cols<-c(cols,confs[[k]]$col)
  }  
  
  legend('bottomright', legend=legend, col=cols, pch=pchs)
  #browser()
  
  if ( pdf ) {
    dev.off()
    cat('See the pdf!\n')
  }
  
  if (!pdf) browser()
}