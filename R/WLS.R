### 加权最小二乘法 y要放在最后一列
WLS <- function(data,W){
  beta <- matrix(c(NA,NA,NA))
  if(nrow(data)>0){
    data <- as.matrix(data)
    W <- as.matrix(W)
    X <- as.matrix(cbind(1,data[,-ncol(data)]))
    Y <- as.matrix(data[,ncol(data)])
    beta <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y
  }
  return(beta)
}
