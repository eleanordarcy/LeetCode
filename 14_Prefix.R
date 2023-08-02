library(strsplit)
library(stringr)

## Extract nth element from each item in a list
fun1 <- function(list, n){
  sapply(list, `[`, n)
}

noteq1 <- function(z) z!=1


prefix <- function(x){
  
  ## Check number of words
  if( length(x) >= 1 & length(x) <= 200 ){
    str <- strsplit(x, split='')
    
    ## Check number of letters in each work
    if( (max(lengths(str))<= 200) & (min(lengths(str))>=0) ){
      
      ## Check all lower case
      if(all(str_detect(unlist(str),"[[:lower:]]")==TRUE) ) {
        
        ## Create array of each element - up to minimum number of letters
        elm <- (fun1(str,1:min(lengths(str))))
        
        ## Find number of unique elements for each position - if 1 then they are all common
        unq <- c()
        for(i in 1:min(lengths(str))){
          unq[i] <- length(unique(elm[i,]))
        }
        if( all(unq==1) ){
          return(paste(elm[1:min(lengths(str)),1], collapse=''))
        } else if( (Position(noteq1, unq) == 1) ){
          return('There is no common prefix among the input strings.')
        } else{
          return(paste(elm[1:(Position(noteq1, unq)-1),1], collapse=''))
        }
      }
      else{
        return('Constraint not satisfied - only lower case letters a-z can be used')
      }
    }
    else{
      return('Constraint not satisfied - each word must contain between 0 and 20 letters')
    }
  } 
  else{
    return('Constraint not satisfied - must input between 1 and 200 words')
  }
}

prefix(c('flower','flow','flowers'))
