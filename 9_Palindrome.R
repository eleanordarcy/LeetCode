rm(list=ls())

palindrome <- function(z) {
  ## z is number to test
  
  ## Change z into a vector of its element
  Z <- as.numeric(strsplit(as.character(z), "")[[1]])
  
  ## If the vector is equal is reverse the elements it is a palindrome
  if( all(Z == rev(Z)) ){
    return(paste0(z, ' is a palindrome'))
  }
  else{
    return(paste0(z, ' is not a palindrome'))
  }
}

palindrome(1666666666666661)

(555 %% c(1e4, 1e3, 1e2, 1e1)) %/% c(1e3, 1e2, 1e1, 1e0)

palindrome2 <- function(z) {
  ## z is number to test
  
  if( z==0 ){
    return(paste0(z, ' is a palindrome'))
  }
  else if ( z != round(z) ){
    return(paste0(z, ' is a not palindrome'))
  }
  else if ( abs(z) != z ){
    return(paste0(z, ' is a not palindrome'))
  }
  else if( z%%10==0){
    return(paste0(z, ' is a not palindrome'))
  }
  else{
    mag <- (ceiling(log10(z)))
    
    seq <- 10^(0:mag)
    seq <- rev(seq)
    seq1 <- seq[1:(length(seq)-1)]
    seq2 <- seq[2:(length(seq))]
    
    ## Change z into a vector of its element
    Z <- (z %% seq1) %/% seq2
    
    
    ## If the vector is equal is reverse the elements it is a palindrome
    if( all(Z == rev(Z)) ){
      return(paste0(z, ' is a palindrome'))
    }
    else{
      return(paste0(z, ' is not a palindrome'))
    } 
  }
}

palindrome2(1666666666666661)
