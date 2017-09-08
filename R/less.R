less <- function(data){
  x <- data[1]
  y <- data[2]
  if(is.na(x)){
    return(FALSE)
  }else{
    if(is.na(y)){
      return(TRUE)
    }else{
      if(x<y){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }
  }
}
