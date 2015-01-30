plot1<-function(run=FALSE,howmany=200,pdf=FALSE) {
  #Figure 5 left in the main paper.
  
  #Settings used
  n<<-6
  N<<-500
  exconf<<-'passive'
  howmany<<-howmany  
  schedule<<-n-2 #do all tests for the new methods
  restrict<<-c('acyclic','sufficient')
  
  #defining the configuations of the algortihms
  confs<<-list()
  confs[[1]]<<-list(test="classic",weight="constant",
                    p=c(0.000001,0.00001,0.0001,0.0005,0.001,0.005,
                        0.01,0.05,0.1,0.15,0.2), #the original run in the paper
                    #p=c(0.0005,0.001,0.005),#a shorter run                    
                    solver="clingo",encode="new_maxindep_acyclic_sufficient.pl",
                    description='hard deps [sec. 4.1]',col=2,pch=20)  
  
  confs[[2]]<<-list(test="classic",weight="constant",
                    p=c(0.001,0.005,0.01,0.03,0.05,0.1,0.15,0.2,0.3),
                    #p=c(0.01,0.05,0.1),
                    solver="clingo",encode="new_wmaxsat_acyclic_sufficient.pl",
                    description='constant weights [sec. 4.2]',col=1,pch=20)  
  
  confs[[3]]<<-list(test="bayes",weight="log",
                    p=c(0.9999,0.999,0.99,0.9,0.7,0.5,0.4,0.3,
                        0.2,0.15,0.1,0.09,0.08,0.07,0.06,0.05,
                        0.04,0.03,0.02,0.01),
                    #p=c(0.2,0.1,0.05),                    
                    solver="clingo",encode="new_wmaxsat_acyclic_sufficient.pl",
                    description='log-weights [sec. 4.3]',col=3,pch=20) 

  confs[[4]]<<-list(test="classic",weight="constant",
                    p=c(0.001,0.01,0.03,0.05,0.1,0.15,0.2,0.3),
                    #p=c(0.01,0.05,0.1),
                    solver="pcalg-pc",encode=NA,
                    description='PC (pcalg)',col=5,pch=1)
  
  confs[[5]]<<-list(test="classic",weight="constant",
                    p=c(0.001,0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.45,0.5,0.6),
                    #p=c(0.01,0.05,0.1,0.15,0.2,0.25),
                    solver="pcalg-cpc",encode=NA,
                    description='cPC (pcalg) (subset)',col=6,pch=1) 
  
  confs[[6]]<<-list(test="classic",weight="constant",
                    p=c(4,3,2,1.5,1,0.8,0.5,0.2),
                    #p=c(2,1.5,1),
                    solver="bnlearn",encode=NA,
                    description='score-based',col=colors()[53],pch=1) 

  confs[[7]]<<-list(test="classic",weight="constant",
                    p=c(0.0001,0.001,0.01,0.03,0.05,0.1,0.15,0.2,0.25),
                    #p=c(0.01,0.05,0.1),
                    solver="test",encode=NA,
                    description='test only',col=4,pch=5)

  #either run the ananlysis or produce a plot
  if ( run ) plot1.run()
  if (!run ) plot1.plot(pdf=pdf)
}  

plot1.run<-function() {

  #make a directory for the plot results
  system('mkdir plot1')
  
  max_time<-0
  for ( i in 1:howmany ) { #looping through all models
    for ( k in 1:7 ) { #all algorithm configurations
      #if ( k == 6 ) next
      for ( p in confs[[k]]$p ) { #all p-thresholds for the given algorithm
        file=paste('plot1/run_',n,'_',k,'_',i,'_',p,'.Rdata',sep='')
        if (file.exists(file)) next #skip if already run
        
        set.seed(i); #set seed to produce the same results
        cat('The seed set to:',i,'\n')
        L<-pipeline(n=n,exconf=exconf,schedule=schedule,
                    test=confs[[k]]$test,p=p,solver=confs[[k]]$solver,
                    encode=confs[[k]]$encode,weight=confs[[k]]$weight,
                    N=N,restrict=restrict,alpha=1.5)
        #save also the true model, which is a global variable
        L$M<-M
        save(L,file=file)
        #browser()
      }
    }
  }
}


plot1.plot<-function(pdf=FALSE) {
  #Function that produces the plot form the calculations saved in the folder.
  
  #collect data for the plot
  legend<-c()
  pchs<-c()
  cols<-c()

  if ( pdf ) {
    pdf( 5,4,file='plot1.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.1) )
  }

  for ( k in 1:7 ) {

    FPvec<-c()
    TPvec<-c()
    FNvec<-c()
    TNvec<-c()
    modelsvec<-c() #calculating how many models considered
    
    for ( p in confs[[k]]$p ) {
      FP<-TP<-FN<-TN<-0
      models<-0
      for ( i in 1:howmany ) {
        file=paste('plot1/run_',n,'_',k,'_',i,'_',p,'.Rdata',sep='')
        if (!file.exists(file)) {
          cat(k,i,p,"NOT RUN YET!!!!\n")
          browser()
          break
        }
        load(file=file)
        if ( is.infinite(L$solving_time) ) {
          cat('RUN skipped!!!\n')
          next
        }
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
    cat(confs[[k]]$description,':',sum(FNvec+TPvec)/length(confs[[k]]$p),'\n')
    print(rbind(confs[[k]]$p,FPRvec,TPRvec,modelsvec))

    #line type is set here individually
    lty<-"solid"
    if ( k==5 ) lty<-"dashed"
  
    if ( k == 1 ) {
      #for the first curve
      plot(c(0,FPRvec,1),c(0,TPRvec,1),xlim=c(0,0.3),
           ylim=c(0.82,0.98),
           type='l',col=confs[[k]]$col,
          xlab='',ylab="",pch=confs[[k]]$pch)
      mtext("FPR",side=1,line=2)
      mtext("TPR",side=2,line=2)
      
      points(FPRvec,TPRvec,col=confs[[k]]$col,pch=confs[[k]]$pch)
      lines(c(0,1),c(0,1))
    } else {
      #for any subsequent curves
      lines(c(0,FPRvec,1),c(0,TPRvec,1),type='l',col=confs[[k]]$col,pch=20,lty=lty)
      points(FPRvec,TPRvec,col=confs[[k]]$col,pch=confs[[k]]$pch)
    }
    if (!pdf) browser()
    
    #gather up some information for printing the legend
    legend<-c(legend,confs[[k]]$description)
    pchs<-c(pchs,confs[[k]]$pch)
    cols<-c(cols,confs[[k]]$col)
  
  }  
  #finally also draw the legend
  legend('bottomright',legend=legend, col=cols, pch=pchs)
  
  if ( pdf ) {
    dev.off()
    cat('See the pdf!\n')
  }
  
  if ( !pdf ) browser()
}