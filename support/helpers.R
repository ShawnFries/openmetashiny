cumfunc<-function(vec){
  cum_vec<-c()
  for(i in 1:length(vec)){
    cum_vec[i]<-sum(vec[1:i])
  }
  cum_vec
}
