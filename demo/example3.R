require(data.table)
require(EBImage)
require(picture)
f3 <- function(x,y){
  z <-cos(4*pi*(1-x-y))-2*cos(4*pi*(1-x-y))*sign(x+y-1>0)
  return(z)
}

test3 <- data.table(x=rep(c(0:127),128),y=rep(c(0:127),each=128))
test3$z <-(f3(test3[["x"]]/128,test3[["y"]]/128)+1)/2
#set.seed(111)
Test3 <- copy(test3)
Test3$z <- test3[['z']]+rnorm(128^2,0,0.2^2)

display(matrix(test3[['z']],128))#原图

display(matrix(Test3[['z']],128))#加噪

time1 <- Sys.time()
hat3_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test3)))
Test3 <- as.data.frame(Test3)
SearchPoints0 <- SearchPoints0(h=5)
for (i in 1:nrow(Test3)){
  print(i)
  hat3_Z[i,] <- LKE_side(m=Test3[i,1],n=Test3[i,2],h=5,
                         data=Test3,
                         SearchPoints0,
                         xmin = 0,
                         xmax = 128,
                         ymin = 0,
                         ymax = 128,
                         scale=128)
}#论文中单边的局部线性核光滑
Sys.time()-time1

result3 <- judge_u(data=hat3_Z,u=0.0002599*8) #选择估计值

display(matrix(result3[,1],128))#局部线性核光滑
display(matrix(result3[,2],128))#大于0部分局部线性核光滑
display(matrix(result3[,3],128))#小于0部分局部线性核光滑
display(matrix(result3[,7],128))#论文中单边局部线性核光滑
