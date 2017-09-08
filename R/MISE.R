##积分均值误差
MISE <- function(x,y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  return(t(x-y)%*%(x-y)/nrow(x))
}

