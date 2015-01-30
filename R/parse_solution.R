parse_solution<-function(solver="clingo",encode=NA,
                         sol_file="tmp/pipeline.ind.clingo") {
  #reads a Clingo outputted solutions and makes a model structure out of it.
  #solver = clingo (other solvers were possible at a point)
  #encode = the ASP encoding file used, for some 
  #sol_file = the file in which the solution is

  M<-list()
  M$G<-array(NA,c(global_n,global_n))
  M$Ge<-array(NA,c(global_n,global_n))
  M$objective <- -1 #for the SAT versions
  
  if ( solver != 'clingo') browser()
  
  sol <- file( sol_file, "r" )
  sol_lines <- readLines(sol,-1)
  close(sol)
  
  if ( solver == 'clingo' ) {
    limit_line <-grep("^TIME LIMIT",sol_lines)
    if ( length(limit_line) != 0 ) {  
      #browser()
      return(NA) #returning NA if clingo was stopped with time out
    }
  }

  #in these cases read the objective function value
  if ( (encode == "asp-maxsat" || encode == "asp-weighted" || 
            encode == "asp-new" || grepl('maxsat',encode) || grepl('maxindep',encode) ) ) {
    #browser()
    found_i<-grep("^OPTIMUM FOUND",sol_lines)
    if (length(found_i) == 0 || sol_lines[found_i] != "OPTIMUM FOUND" ) {
      cat('Something wrong with Clingo output!!!\n')
      browser()
    } else {
      cat('\t\tREAD:\"',sol_lines[found_i],'\"\n')
    } 
    opt_i<-max(grep("^Optimization",sol_lines))
    #M$objective<-as.numeric(strsplit(sol_lines[opt_i]," ")[[1]][2]) #for old clingo
    
    M$objective<-as.numeric(strsplit(sol_lines[opt_i]," ")[[1]][3])
    cat('\t\tREAD:\"',sol_lines[opt_i],'\"\n')
    #browser()
  } 
  
  sol_i<-max(grep("^Answer",sol_lines))+1
  sol<-strsplit(sol_lines[sol_i],' ')[[1]]

  for ( s in sol) { #reading of the graph in the solution
    if ( substr(s,1,5) =="edge(") {
      vars<-as.numeric(unlist(strsplit(substr(s,6,nchar(s)-1),',')))
      M$G[vars[2],vars[1]]<-1
    } else if ( substr(s,1,5) =="conf(") {
      vars<-as.numeric(unlist(strsplit(substr(s,6,nchar(s)-1),',')))
      M$Ge[vars[2],vars[1]]<-M$Ge[vars[1],vars[2]]<-1        
    }
  }
  M$G[is.na(M$G)]<-0
  M$Ge[is.na(M$Ge)]<-0
  cat('\t\tREAD:',substr(sol_lines[sol_i],1,30),'...',substr(sol_lines[sol_i],nchar(sol_lines[sol_i])-30,nchar(sol_lines[sol_i])),'\n')  
  
  #return the model
  M
}







