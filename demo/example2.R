require(EBImage)
require(SSLKE)

f2 <- function(x,y){
  z <- 0.25*(1-x)*y+(1+0.2*sin(2*pi*x))*sign(y>0.6*sin(pi*x)+0.2)
  return(z)
}

test2 <- data.frame(x=rep(c(0:127),128),y=rep(c(0:127),each=128))
test2$z <-f2(test2[["x"]]/128,test2[["y"]]/128)/1.31415
#set.seed(111)
Test2 <- test2
Test2$z <- test2[['z']]+rnorm(128^2,0,0.2^2)

display(matrix(test2[['z']],128))#原图

display(matrix(Test2[['z']],128))#加噪

time1 <- Sys.time()
hat2_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test2)))
Test2 <- as.data.frame(Test2)
SearchPoints0 <- SearchPoints0(h=5)
for (i in 1:nrow(Test2)){
  #print(i)
  hat2_Z[i,] <- LKE_side(m=Test2[i,1],n=Test2[i,2],h=5,
                        data=Test2,
                        SearchPoints0,
                        xmin = 0,
                        xmax = 128,
                        ymin = 0,
                        ymax = 128,
                        scale=128)
}#论文中单边的局部线性核光滑
Sys.time()-time1

result2 <- judge_u(data=hat2_Z,u=0.0002599*4)$data #选择估计值


display(matrix(result2[,1],128))#局部线性核光滑
display(matrix(result2[,2],128))#大于0部分局部线性核光滑
display(matrix(result2[,3],128))#小于0部分局部线性核光滑
display(matrix(result2[,7],128))#论文中单边局部线性核光滑

MISE(result2[,7],test2[,3]) #计算积分均方误差
