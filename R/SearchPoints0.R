### 根据窗宽寻找邻域内的点的坐标
#(0,0)中心点坐标，h窗宽,interval坐标间隔,无边界
#requireNamespace('data.table')
#requireNamespace('dplyr')
SearchPoints0 <- function(h,interval=1){
  #require(data.table)
  #require(dplyr)
  n <- floor(h/interval)
  sapply(c(1:n),function(x) floor(sqrt(h^2-x^2)))
  PointsPosition0 <- data.table()
  for(i in 0:n){
    H <- floor(sqrt(h^2-i^2))
    PointsPosition0 <- data.table(x=rep(i,(H+1)),y=seq(0,H,by=1))%>%
      rbind(PointsPosition0,.)
  }
  PointsPosition0 <- data.table(x=PointsPosition0$x,
                                y=-PointsPosition0$y)%>%rbind(PointsPosition0,.)
  PointsPosition0 <- data.table(x=-PointsPosition0$x,
                                y=PointsPosition0$y)%>%rbind(PointsPosition0,.)
  PointsPosition0 <- unique.data.frame(PointsPosition0)   # 去重
  rownames(PointsPosition0) <- c(1:nrow(PointsPosition0))
  return(PointsPosition0)
}
