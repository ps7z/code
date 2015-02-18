represent_graph<-function(filename1=NA,filename2=NA,adjacency=0,confounders=NA,default_know=95,default_maybe=80,default_not=95){
   if (!is.na(filename1)) graphfile = file(filename1,"w")
   if (!is.na(filename2)) memberfile = file(filename2,"w")
   n = dim(adjacency)[1]
   pathmat = adjacency
   multmanip1 = list()
   multmanip1a = list()
   cat(file=memberfile,"node(1..",n,").\n")
   cat(file=memberfile,"cset(0).\n")
   cat(file=memberfile,"jset(0).\n")
   if (is.matrix(adjacency)) {
     for (i in 1:n) {
      for (j in 1:n){
        if (i != j) {
            x = 0
            for (k in 1:n){
               if (k != j) x = x + 2^(k-1)}
     if (!is.element(x,multmanip1)) {
         multmanip1 = append(x,multmanip1)
         multmanip1a = append(j,multmanip1a)}
     if (adjacency[i,j] == 0)
           cat(file=graphfile,"indep(",min(i,j),",",max(i,j),",",0,",",x,",",default_not,").","\n")
     else if (adjacency[i,j] == 1)
                 cat(file=graphfile,"dep(",min(i,j),",",max(i,j),",",0,",",x,",",default_know,").","\n")
     else cat(file=graphfile,"indep(",min(i,j),",",max(i,j),",",0,",",x,",",default_maybe,").","\n")}}}}
     for (k in 1:n)
         for (i in 1:n)
             for (j in 1:n)
                 if ((k != i) && (k != j) && (i != j) && (pathmat[i,k] == 1) && (pathmat[k,j] == 1)) pathmat[i,j] = 1
     for (i in 1:n) {
      for (j in 1:n){
          if (i != j) {
           temp = 2^(i-1)
           if (pathmat[i,j] == 1) cat(file=graphfile,"dep(",min(i,j),",",max(i,j),",",0,",",temp,",",default_know,").","\n") 
           else if (pathmat[i,j] == 0) cat(file=graphfile,"indep(",min(i,j),",",max(i,j),",",0,",",temp,",",default_not,").","\n")                    else cat(file=graphfile,"indep(",min(i,j),",",max(i,j),",",0,",",temp,",",default_maybe,").","\n")
       }}}
   for (i in 1:n){
       cat(file=memberfile,"jset(",multmanip1[[i]],").\n")
       for (j in 1:n) if (j != multmanip1a[[i]]) cat(file=memberfile,"ismember(",multmanip1[[i]],",",j,").\n")
       cat(file=memberfile,"jset(",2^(i-1),").","   ismember(",2^(i-1),",",i,").\n")}
   if (!is.na(filename1)) close(graphfile)
   if (!is.na(filename2)) close(memberfile)
}
        
   
