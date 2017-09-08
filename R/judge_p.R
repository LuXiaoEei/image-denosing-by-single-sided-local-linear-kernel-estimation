judge_p <- function(data,u){
  point <- rep(1,nrow(data))
  diff<- apply(data.frame(data$X4-data$X5,data$X4-data$X6),1,max,na.rm = TRUE)
  point[diff<=u]  <- 0
  return(point)
}

