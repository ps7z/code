compare<-function(L,M,verbose=TRUE,passive=FALSE) {
  #Compares the learned moded L to the true model M
  #Uses the global_indeps, the global variable list with all ind. test results.

  added<-0 #(added dependence) look for these
  deleted<-0 #(deleted dependence)
  totaldeps<-0
  totalindeps<-0
  
  for ( indep in global_indeps) { #
    if ( ( indep$jset != 0 || indep$jset >= (2^global_n) ) && passive ) next;
    
    #calculate the result in the true model
    independent_M<-!directed_reachable(indep$vars[1],indep$vars[2],indep$C,indep$J,M)
    
    if ( is.null(L$G) ) { #for test only, the learned state is the result of the test
      independent_L<-indep$independent
    } else { #otherwise it is read of the graph for the learned model L
      independent_L<-!directed_reachable(indep$vars[1],indep$vars[2],indep$C,indep$J,L)      
    }
    
    if ( independent_M && !independent_L ) { #L added a depedence 
      added<-added+1 
    } else if (independent_L && !independent_M ) { #L deledeted and independence
      deleted<-deleted+1
    }
    
    if ( independent_M ) { #totals
      totalindeps<-totalindeps+1;
    } else {
      totaldeps<-totaldeps+1;
    }
    
    #finallyt some printing if needed
    true_mark <- '_||_'
    if ( !independent_M ) true_mark <- '_N_'
    
    learned_mark <- '_||_'
    if (!independent_L ) learned_mark <- '_N_'
      
    if ( verbose && learned_mark != true_mark ) cat(indep$vars[1],'?',indep$vars[2],'|',paste(indep$C,collapse=','),
                        '||',paste(indep$J,collapse=','),
                        'true:',true_mark,'learned:',learned_mark,'suffered loss',indep$w,'\n')
  }
  
  list(added=added,deleted=deleted,totaldeps=totaldeps,totalindeps=totalindeps)  
}