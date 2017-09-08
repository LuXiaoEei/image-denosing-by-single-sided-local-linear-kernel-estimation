### local kernel estimator 拒不线性核估计
#requireNamespace('data.table')
LKE <- function(m,n,h,
                data,
                SearchPoints0,
                kernel='bivariate kernal',
                xmin=-1000,
                xmax=1000,
                ymin=-1000,
                ymax=1000,
                scale){
  #require(data.table)
  data <- as.data.table(data)
  Area <- SearchPoints(m,n,SearchPoints0,xmin,xmax,ymin,ymax)#寻找邻域内的点
  Area <-merge(Area,data,by=c("x","y"))#从输入数据中匹配出邻域内的点的各值
  #if(kernel=='bivariate kernal'){
  Area$x <-  Area$x-m
  Area$y <-  Area$y-n
  Area$K <- BivKernal(x=Area[['x']]/scale,y=Area[['y']]/scale,h=h/scale)#计算核的值，权重
  #}

  Area <- as.data.frame(Area)
  beta <- WLS(Area[,-ncol(Area)],W=diag(Area$K))#加权最小二乘法
  return(beta)
  }

