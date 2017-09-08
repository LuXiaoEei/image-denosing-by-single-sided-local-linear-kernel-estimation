require(data.table)
require(EBImage)
require(SSLKE)

# 模拟函数
f1 <- function(x,y){
  z <- -2*(x-0.5)^2-2*(y-0.5)^2+sign((x-0.5)^2+(y-0.5)^2<0.25^2)
return(z)
}
# 生成数据集
test <- data.table(x=rep(c(0:99),100),y=rep(c(0:99),each=100))
test$z <-(f1(test[["x"]]/100,test[["y"]]/100)+1)/2
#set.seed(111)
Test <- copy(test)

# 加噪声
Test$z <- test[['z']]+rnorm(10000,0,0.2^2)

# 可视化image
display(matrix(test[['z']],100))#原图
display(matrix(Test[['z']],100))#加噪

# 去燥实现
# time1 <- Sys.time()
hat_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test)))
Test <- as.data.frame(Test)
SearchPoints0 <- SearchPoints0(h=5)
for (i in 1:nrow(Test)){
  # print(i)
  hat_Z[i,] <- LKE_side(m=Test[i,1],n=Test[i,2],h=5,
                  data=Test,
                  SearchPoints0,
                  xmin = 0,
                  xmax = 100,
                  ymin = 0,
                  ymax = 100,
                  scale=100)
}#论文中单边的局部线性核光滑
# Sys.time()-time1

result <- judge_u(data=hat_Z,u=0.0002599*4) #选择估计值
# Rprof(NULL)
# summaryRprof("Rprof-mem.out", memory="both")


display(matrix(result[,1],100))#局部线性核光滑
display(matrix(result[,2],100))#大于0部分局部线性核光滑
display(matrix(result[,3],100))#小于0部分局部线性核光滑
display(matrix(result[,7],100))#论文中单边局部线性核光滑

MISE(result[,7],test[,3]) #计算积分均方误差

