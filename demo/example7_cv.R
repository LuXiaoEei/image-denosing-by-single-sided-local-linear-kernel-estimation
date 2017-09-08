## CV
require(data.table)
require(EBImage)
require(picture)
f2 <- function(x,y){
  z <- 0.25*(1-x)*y+(1+0.2*sin(2*pi*x))*sign(y>0.6*sin(pi*x)+0.2)
  return(z)
}

test2 <- data.table(x=rep(c(0:127),128),y=rep(c(0:127),each=128))
test2$z <-f2(test2[["x"]]/128,test2[["y"]]/128)/1.31415
#set.seed(111)
Test2 <- copy(test2)
Test2$z <- test2[['z']]+rnorm(128^2,0,0.2^2)

display(matrix(test2[['z']],128))#原图

display(matrix(Test2[['z']],128))#加噪




#cv1 <- function(h,Test2=Test2){
cv1 <- function(h){
#require(picture)
hat2_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test2)))
Test2 <- as.data.frame(Test2)
SearchPoints0 <- SearchPoints0(h)[-(x==0&y==0)]###cv
for (i in 1:nrow(Test2)){
  #print(i)
  hat2_Z[i,] <- tryCatch(
    LKE_side(m=Test2[i,1],n=Test2[i,2],h,
                         data=Test2,
                         SearchPoints0,
                         xmin = 0,
                         xmax = 128,
                         ymin = 0,
                         ymax = 128,
                         scale=128),
    warning=function(w){
      print(list(h,i,w))
    })
  ### mn对应坐标，h窗宽，data数据集，
  ### SearchPoints0原电处的邻域内的点集（cv的话，这个点集不包含远点），
  ### 下面四个是边界，scale是尺度调整，把0-128的点映射到0-1上
}#论文中单边的局部线性核光滑
return(hat2_Z)
}

cv2 <- function(i,j){
  x <- MISE(judge_u(res[[i]],u=0.0002599*j)[,7],test2[,3])
  #print(i+j)
  return(c(i,j,x))
}

#display(matrix(data = judge_u(res[[2]],u=0.0002599*1)[,7],nrow = 128))

time1 <- Sys.time()
#cl <- makeCluster(getOption('cl.cores',3))
#res <- parLapply(cl,1:10,cv1,Test2)
#stopCluster(cl)
res <- lapply(c(2:11),cv1)
fin <- mapply(cv2,i=rep(c(1:10),each=200),j=rep(c(seq(0.01,1,0.01),2:101),10))
fin[,which(fin[3,]==min(fin[3,])),drop=FALSE]
Sys.time()-time1
p <- ggplot(z,aes(y=-log10(X3),x=X2))
p+geom_line(aes(color=factor(X1)))

result2 <- judge_u(data=res[[1]],u=0.0002599)
result2 <- judge_u(data=hat2_Z,u=0.0002599*4) #选择估计值
#计算积分均方误差
MISE(result2[,7],test2[,3])

display(matrix(result2[,1],128))#局部线性核光滑
display(matrix(result2[,2],128))#大于0部分局部线性核光滑
display(matrix(result2[,3],128))#小于0部分局部线性核光滑
display(matrix(result2[,7],128))#论文中单边局部线性核光滑





display(matrix(result2[,7],128)-matrix(test2[['z']],128))

# time1 <- Sys.time()
# hat2_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test2)))
# Test2 <- as.data.frame(Test2)
# SearchPoints0 <- SearchPoints0(h=5)[-(x==0&y==0)]###cv
# for (i in 1:nrow(Test2)){
#   #print(i)
#   hat2_Z[i,] <- tryCatch(
#     LKE_side_cv(m=Test2[i,1],n=Test2[i,2],h=5,
#                 data=Test2,
#                 SearchPoints0,
#                 xmin = 0,
#                 xmax = 128,
#                 ymin = 0,
#                 ymax = 128,
#                 scale=128),
#     warning = function(w) {print(list(i,w))}
#
#   )
# }#论文中单边的局部线性核光滑
# Sys.time()-time1


h=2
i=1
m=Test2[i,1]
n=Test2[i,2]
data=Test2
SearchPoints0 <- SearchPoints0(h)[-(x==0&y==0)]
xmin = 0
xmax = 128
ymin = 0
ymax = 128
scale=128


# [1]   2 128
# [1]     2 16257
# [1]     2 16384
# [1] 3 1
# [1]   3 128
# [1]     3 16257
# [1]     3 16384
# [1] 4 1


a <- Sys.time()
t <- judge_u(hat2_Z,u=0.0002599*1)
Sys.time()-a
b <- Sys.time()
MISE(t[,7],test2[,3])
c <- Sys.time()
b-a
c-b


a <- Sys.time()
i=2
LKE_side(m=Test2[i,1],n=Test2[i,2],h,
         data=Test2,
         SearchPoints0,
         xmin = 0,
         xmax = 128,
         ymin = 0,
         ymax = 128,
         scale=128)
Sys.time()-a


Rprof("Rprof.out")
t <- judge_u(hat2_Z,u=0.0002599*1)
Rprof(NULL)
summaryRprof("Rprof.out")
