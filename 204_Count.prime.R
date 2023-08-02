rm(list=ls())

## subfunction for is a number a prime
## input is a number p < x
is.prime <- function(p){
  if(p==1){
    return(TRUE)
  }
  else{
    if(sum(sapply(1:p, function(x) p%%x==0)) == 2){
      return(TRUE)
    }
    else if(sum(sapply(1:p, function(x) p%%x==0)) > 2){
      return(FALSE)
    }
    else{
      return(NA)
    }
  }
}
## output logical T/F (prime/not prime)

## input x is number in question
count.primes <- function(x){
  if(x <= 0){
    return("Invalid input")
  }
  else{
    x <- ceiling(x)
    X <- 1:(x-1)
    return(paste0('There are ', sum(sapply(X, function(v) is.prime(v))), ' primes less than ', x, 
                '. They are: ', paste0(X[c(sapply(1:(x-1), function(v) is.prime(v))==T)], collapse=', '), '.'))
  }
}
## output n is number of primes less than n

start <- Sys.time()
count.primes(499979)
(Sys.time() - start)
