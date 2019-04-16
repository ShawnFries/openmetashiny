cumfunc <- function(vec){
  cum_vec <- c()
  cum_length <- length(vec)
  for (i in 1:cum_length) {
    cum_vec[i] <- sum(vec[1:i])
  }
  cum_vec
}
