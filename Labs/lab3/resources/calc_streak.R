calc_streak <- function(x){
  # check for whether x is a variable in a data frame
  # or an atomic vector
  if(!is.atomic(x)){x <- x[,1]}
  y <- rep(0,length(x))
  y[x == "H"] <- 1
  y <- c(0, y, 0)
  wz <- which(y == 0)
  streak <- diff(wz) - 1
  #return(streak)
  return(data.frame(length = streak))
}