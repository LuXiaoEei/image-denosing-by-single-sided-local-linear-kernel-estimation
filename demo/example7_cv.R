## CV
require(data.table)
require(EBImage)
require(SSLKE)

f2 <- function(x,y){
  z <- 0.25*(1-x)*y+(1+0.2*sin(2*pi*x))*sign(y>0.6*sin(pi*x)+0.2)
  return(z)
}

test2 <- data.frame(x=rep(c(0:255),256),y=rep(c(0:255),each=256))
# test2$z <-f2(test2[["x"]]/256,test2[["y"]]/256)/max(f2(test2[["x"]]/256,test2[["y"]]/256))
test2$z <-f2(test2[["x"]]/256,test2[["y"]]/256)
#set.seed(111)
Test2 <- test2
Test2$z <- test2[['z']]+rnorm(256^2,0,0.5)#加噪

display(matrix(test2[['z']],256))#原图
display(matrix(Test2[['z']],256))#加噪



cv1 <- function(h){
  hat2_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test2)))
  Test2 <- as.data.frame(Test2)
  SearchPoint0 <- subset(SearchPoints0(h),x!=0|y!=0)###cv
for (i in 1:nrow(Test2)){
  # print(i)
  hat2_Z[i,] <- tryCatch(
    LKE_side(m=Test2[i,1],n=Test2[i,2],h,
             data=Test2,
             SearchPoint0,
             xmin = 0,
             xmax = 255,
             ymin = 0,
             ymax = 255,
             scale= 256),
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
  x <- MISE(judge_u(res[[i]],u=0.00025255*j)$data[,7],test2[,3])
  return(c(i,j,x))
}

time2 <- Sys.time()
Sys.time()
res <- lapply(c(2:11),cv1)  #对h遍历
Sys.time()-time2
fin <- mapply(cv2,i=rep(c(1:10),each=200),j=rep(c(seq(0.01,1,0.01),2:101),10))# 每个h对u遍历
Sys.time()-time2
MIN <- fin[,which(fin[3,]==min(fin[3,])),drop=FALSE] #选出最小残差平方和的组合
Sys.time()-time2

result2 <- judge_u(data=res[[MIN[1]]],u=0.00025255*MIN[2])$data #选择估计值
jumps <- judge_u(data=res[[MIN[1]]],u=0.00025255*MIN[2])$jumps

display(matrix(result2[,1],256))#局部线性核光滑
display(matrix(result2[,2],256))#大于0部分局部线性核光滑
display(matrix(result2[,3],256))#小于0部分局部线性核光滑
display(matrix(result2[,7],256))#论文中单边局部线性核光滑

# save.image(file = 'restult256.RData')


display(matrix(result2[,7],256))
display(matrix(result2[,7],256)-matrix(test2[['z']],256))


MISE <- MISE(result2[,7,drop=F],test2[,3,drop=F]) #计算积分均方误差

hfin <- MIN[1]
SearchPoint0 <- SearchPoints0(h = hfin)
JumpsPost <- test2[jumps,1:2]
Jumps <- data.frame()
for (index in 1:nrow(JumpsPost)){
  m <- JumpsPost[index,1]
  n <- JumpsPost[index,2]
  tmp <- SearchPoints(m,n,SearchPoint0,xmin=0,xmax=255,ymin=0,ymax=255)
  Jumps <- rbind(Jumps,tmp)
}
Jumps <- as.matrix(unique.data.frame(Jumps))
MISEe <- MISE(matrix(result2[,7],256)[Jumps],matrix(test2[['z']],256)[Jumps])
save.image(paste0('.//demo//',MISE*10^4,'.Rdata'))

# lxl <- matrix(0,256,256)
# lxl[as.matrix(JumpsPost)] <- 1
# display(lxl)
