f4 <- function(data) {
  x=data[1]
  y=data[2]
  if (0 <= x&x < 15) {
    z = 0
  } else{
    if (15 <= x&x < 30) {
      z = 1
    } else{
      if (30 <= x&x < 45) {
        z = 0.75
      } else{
        if (45 <= x&x < 60) {
          z = 0.5
        } else{
          if (60 <= x&x < 80) {
            z = 0
          } else{
            if (80 <= x&x < 110) {
              z = 2 / 75 * x - 32 / 15
            } else{
              if (110 <= x&x < 140) {
                z = -2 / 75 * x + 220 / 75 + 0.8
              } else{
                if (140 <= x&x < 200) {
                  z = 1/(65-0.3*y)^2*(x-140-(0.3*y-5))^2
                } else{
                  z = -1/(65-0.3*y)^2*(x-200-(0.3*y-5))^2+1
                }
              }
            }
          }
        }
      }
    }
  }
return(z)
}

require(data.table)
require(EBImage)

test4 <- data.frame(x=rep(c(0:259),32),y=rep(c(0:31),each=260))
test4$z <- apply(test4,1,f4)
#set.seed(111)
#Test4 <- copy(test4)
Test4$z <- test4[,3]+rnorm(260*32,0,0.2^2)

display(matrix(test4[['z']],260))#原图

display(matrix(Test4[['z']],260))#加噪

time1 <- Sys.time()
hat4_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test4)))
Test4 <- as.data.frame(Test4)
SearchPoints0 <- SearchPoints0(h=4)
for (i in 1:nrow(Test4)){
  print(i)
  hat4_Z[i,] <- LKE_side(m=Test4[i,1],n=Test4[i,2],h=4,
                         data=Test4,
                         SearchPoints0,
                         xmin = 0,
                         xmax = 260,
                         ymin = 0,
                         ymax = 32,
                         scale= 260)
}#论文中单边的局部线性核光滑
Sys.time()-time1

result4 <- judge_u(data=hat4_Z,u=0.0002599*5) #选择估计值
result4_p <- judge_p(data=hat4_Z,u=0.0002599*5) #寻找边界

display(matrix(result4[,1],260))#局部线性核光滑
display(matrix(result4[,2],260))#大于0部分局部线性核光滑
display(matrix(result4[,3],260))#小于0部分局部线性核光滑
display(matrix(result4[,7],260))#论文中单边局部线性核光滑
display(matrix(result4_p,260))#边界

MISE(result4[,7],test4[,3]) #计算积分均方误差

