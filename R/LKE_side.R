### local kernel estimator 局部线性核估计 针对边缘采用分割支撑集的方式

LKE_side <- function(m,n,h,
                data,
                SearchPoints0,
                kernel='bivariate kernal',
                xmin=-1000,
                xmax=1000,
                ymin=-1000,
                ymax=1000,
                scale){
  data <- as.data.table(data)
  Area <- SearchPoints(m,n,SearchPoints0,xmin,xmax,ymin,ymax)
  Area <-merge(Area,data,by=c("x","y"))
  #if(kernel=='bivariate kernal'){
  Area$x <-  Area$x-m
  Area$y <-  Area$y-n
  Area$K <- BivKernal(x=Area[['x']]/scale,y=Area[['y']]/scale,h=h/scale)
  #}
  Area <- as.data.frame(Area)
  beta <- WLS(Area[,-ncol(Area)],W=diag(Area$K))#拟合值，梯度

  beta <- rbind(beta,WRMS(Z = Area$z,
                          a=beta[1,1],
                          x_grad = beta[2,1],
                          X = Area$x,
                          y_grad = beta[3,1],
                          Y = Area$y,
                          W = Area$K))

  Area$sep <- beta[2,1]*Area[,1]+beta[3,1]*Area[,2]#计算在切线上下
  Area1 <- subset(Area,sep>=0)
  Area2 <- subset(Area,sep<=0)#支撑集分割

  #一以下对于报错一方面是某类只有一个点，或者某类的所有点的某个维度的坐标相等导致矩阵不可逆，这里为了计算的效率，采用NA，这不影响最后的结果。
  beta1 <- tryCatch({
    WLS(Area1[,c('x','y','z')],W=diag(Area1$K,nrow=length(Area1$K)))
    },error=function(e){
      #WLS(Area2[,c('x','y','z')],W=diag(Area2$K))
      matrix(c(NA,NA,NA))
    })

  beta1 <- rbind(beta1,WRMS(Z = Area1$z,
                            a = beta1[1,1],
                            x_grad = beta1[2,1],
                            X = Area1$x,
                            y_grad = beta1[3,1],
                            Y = Area1$y,
                            W = Area1$K))

  beta2 <-tryCatch({
    WLS(Area2[,c('x','y','z')],W=diag(Area2$K,nrow=length(Area2$K)))
  },error=function(e){
    #beta1
    matrix(c(NA,NA,NA))
  })

  beta2 <- rbind(beta2,WRMS(Z = Area2$z,
                            a=beta2[1,1],
                            x_grad = beta2[2,1],
                            X = Area2$x,
                            y_grad = beta2[3,1],
                            Y = Area2$y,
                            W = Area2$K))
rownames(beta) <- NULL
rownames(beta1) <- NULL
rownames(beta2) <- NULL

result <- data.frame(beta,beta1,beta2)

return(cbind(result[1,],result[4,]))#返回三个估计值对应的梯度和WRMS
}

