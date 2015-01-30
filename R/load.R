#These are some of the parameters needed for operation
#

#loads the whole directory to R
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

# since the functions here are not really changed, loading them already here
loud<-function() {

  library('deal') #used in the bayes independence test
  library('pcalg') #for running pc derivative algorithms
  library('combinat')
  library('graph') #for pcalg output handling
  library('bnlearn') #score-based learning
#  cat('Loading the R-code...\n')
  sourceDir('./',trace=FALSE)
  sourceDir('./mods/',trace=FALSE)

#sourceDir('./new/',trace=FALSE)

#  cat('Done.\n')
}


