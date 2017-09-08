judge_u <- function(data,u){
  data$final <- data$X1
  diff<- apply(data.frame(data$X4-data$X5,data$X4-data$X6),1,max,na.rm = TRUE)
  comp <- apply(data[,5:6],1,less)
  data$final[diff>u&comp] <- data$X2[diff>u&comp]
  # data$final[diff>u&data$X5==data$X6] <- (data$X2[diff>u&data$X5==data$X6]+
  #   data$X3[diff>u&data$X5==data$X6])/2
  data$final[diff>u&!comp] <- data$X3[diff>u&!comp]
  return(data)
  }

