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



cv1 <- function(h){
hat2_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test2)))
Test2 <- as.data.frame(Test2)
SearchPoints0 <- SearchPoints0(h)[-(x==0&y==0)]###cv
for (i in 1:nrow(Test2)){
  print(i)
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
  print(i+j)
  return(c(i,j,x))
}

res <- lapply(c(2:11),cv1)  #对h遍历
fin <- mapply(cv2,i=rep(c(1:10),each=200),j=rep(c(seq(0.01,1,0.01),2:101),10))# 每个h对u遍历
MIN <- fin[,which(fin[3,]==min(fin[3,])),drop=FALSE] #选出最小残差平方和的组合


result2 <- judge_u(data=res[[MIN[1]]],u=0.0002599*MIN[2]) #选择估计值

display(matrix(result2[,1],128))#局部线性核光滑
display(matrix(result2[,2],128))#大于0部分局部线性核光滑
display(matrix(result2[,3],128))#小于0部分局部线性核光滑
display(matrix(result2[,7],128))#论文中单边局部线性核光滑





display(matrix(result2[,7],128)-matrix(test2[['z']],128))





