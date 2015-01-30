plot4<-function(run=FALSE,howmany=1000,pdf=FALSE) {
  #Figure 7 in the main paper.
  #(I think the original figure in the paper may have 
  # the heckerman/bottcher prior differently as this one. Since the plot
  # looks a tiny bit different.)
  
  n<<-4
  N<<-seq(50,howmany,by=100)
  howmany<<-howmany
  
  topology<<-"example" #topology for the network that is used.
  
  confs<<-list()

  confs[[1]]<<-list(test="bayes",weight="log",p=0.5,solver="clingo",encode="new_wmaxsat.pl",description='bayes-log weights (new)',schedule=n-2,alpha=20)
  #note that the alpha was added afterwards, previously it was hard coded
    
  if ( run ) plot4.run()
  if ( !run ) plot4.plot(pdf)
}  

plot4.run<-function() {
  
  system('mkdir plot4')
  
  for ( i in N ) {
    for ( k in 1:100 ) {

      file=paste('plot4/run_',n,'_',k,'_',i,'.Rdata',sep='')
      if (file.exists(file)) next
      
      #always set seed to 0 in this test
      set.seed(k); #set seed to produce the same results
      cat('The seed set to:',k,'\n')
      L<-pipeline(n=n,topology="example",exconf=exconf,
                  schedule=confs[[1]]$schedule,
                  test=confs[[1]]$test,p=confs[[1]]$p,
                  solver=confs[[1]]$solver,
                  alpha=confs[[1]]$alpha,
                  encode=confs[[1]]$encode,weight=confs[[1]]$weight,
                  N=i,restrict=c('sufficient')) 
      #restrict applies only for the model!, the encode file
      #allows for cycles and latents!!!!
      
      #finally check the input quality
      #this uses the global_indeps list of tested indeps
      L$test_quality<-test_quality(M)
      #save also the true model
      L$M<-M
      save(L,file=file)
    }
  }
}


plot4.plot<-function(pdf) {  
  
  legend<-c()
  if ( pdf ) {
    pdf( 7,3,file='plot4.pdf')
    par( omi=c(0,0,0,0),mai=c(0.6,0.6,0.1,0.9) )
  }
  image<-array(0,c(24,length(N)))
  error<-rep(0,length(N))
  error2<--rep(0,length(N))
  objective<-c()
  ix<-c()
  str<-NULL
  for ( k in 1:100) {
    for ( i in N ) {
      file<-paste('plot4/run_',n,'_',k,'_',i,'.Rdata',sep='')
      cat(file,'\n')
      if (!file.exists(file)) next
      load(file=file)

      image[,which(i==N)]<-image[,which(i==N)]+L$test_quality$binloss

      objective<-c(objective,L$objective)
      
      error[which(i==N)]<-error[which(i==N)]+(L$added+L$deleted)
      error2[which(i==N)]<-error2[which(i==N)]+sum(L$test_quality$binloss)
      
      if ( is.null(str)) str<-L$test_quality$str
      #browser()
    }
  }
  
  iix<-sort(rowSums(image),index.return=TRUE,decreasing=TRUE)$ix[1:10] #[1:100]
  
  
  image<-image[iix,]
  str<-str[iix]
  
  ix<-N-50
  image(x=N,y=(1:nrow(image)),1-t(image),xlab="",ylab='')
  
  mtext("sample size",side=1,line=2)
  mtext("# (in)dependence errors",side=2,line=2)
  
  axis(4,at=1:nrow(image),labels=str,las=1,tick=TRUE,hadj=0.1)
  lines(ix+50,error/100+0.5,lwd=3)
  lines(ix+50,error2/100+0.5,lwd=3,col='blue')  
  
  
  legend('topright',c('errors in input','errors in output'),
         col=c('blue','black'),pch=20,bg='white')
  
  if ( pdf ) {
    dev.off()
    cat('See the pdf!\n')
  }
  
  if (!pdf) browser()
  
}