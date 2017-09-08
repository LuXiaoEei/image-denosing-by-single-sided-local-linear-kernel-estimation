require(data.table)
require(EBImage)
f8 <- function(x){
  z <-20*sign(x>0&x<100)+(0.2*x)*sign(x>100&x<200)+10*sign(x>200&x<300)+70*sign(x>300&x<400)+60*sign(x>400&x<500)
  return(z)
}

test8 <- data.table(x=c(1:500),y=1)
test8$z <-f8(test8[["x"]])
#set.seed(111)
Test8 <- copy(test8)
Test8$z <- test8[['z']]+rnorm(500,0,4)
require(ggplot2)
ggplot(Test8,aes(x=x,y=z))+geom_point()



# cv1 <- function(h){
# #require(picture)
# hat8_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test8)))
# Test8 <- as.data.frame(Test8)
# SearchPoints0 <- SearchPoints0(h)[-(x==0&y==0)]###cv
# for (i in 1:nrow(Test8)){
#   #print(i)
#   hat2_Z[i,] <- LKE_side(m=Test8[i,1],n=Test8[i,2],h,
#                          data=Test8,
#                          SearchPoints0,
#                          xmin = 1,
#                          xmax = 500,
#                          ymin = 1,
#                          ymax = 1,
#                          scale=500)
#   ### mn对应坐标，h窗宽，data数据集，
#   ### SearchPoints0原电处的邻域内的点集（cv的话，这个点集不包含远点），
#   ### 下面四个是边界，scale是尺度调整，把0-128的点映射到0-1上
# }#论文中单边的局部线性核光滑
# return(hat2_Z)
# }
#
#
# m=Test8[i,1]
# n=Test8[i,2]
# h
#                          data=Test8
#                          SearchPoints0 <- SearchPoints0(h)[-(x==0&y==0)]###cv
#                          xmin = 1
#                          xmax = 500
#                          ymin = 1
#                          ymax = 1
#                          scale=500
