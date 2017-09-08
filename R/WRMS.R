##  W：加权矩阵，Z：观测值向量，a：拟合值，x_grad：x梯度，X：x值向量，y_grad：y梯度，Y：y值向量
WRMS <- function(Z,a,x_grad,X,y_grad,Y,W){
  M <- Z-a-x_grad*X-y_grad*Y
  #M <- as.matrix(Z-a-x_grad*X-y_grad*Y)
  return(1/sum(W)*t(M)%*%diag(W,nrow=length(W))%*%M)
  }


