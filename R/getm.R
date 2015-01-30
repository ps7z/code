getm<-function(vars,C) {
  #gives a integer representation of the M set given number of variables and conditioning set C
  vec<-rep(1,global_n)
  #vec[x]<-vec[y]<-0
  vec[vars]<-0
  vec[C]<-0
  #bin.to.dec(vec)
  bin.to.dec(rev(vec))
}