myFirstRFunction <- function(n) {
  #return sumof(0:n):n/2 OR n/7 
  sum <- 0
  inc <- function(x, y)
  {
    eval.parent(substitute(x <- x + y))
  }
  stopifnot(is.numeric(n), n>=0)
  if(n<2){
    return(sum)
  }else{
    for(i in 2:n-1){
      if(i%%2 == 0 | i%%7 == 0){
        sum <- inc(sum,i)
      }
      print(c(i,sum))
    }
  }
}
myFirstRFunction(1000)