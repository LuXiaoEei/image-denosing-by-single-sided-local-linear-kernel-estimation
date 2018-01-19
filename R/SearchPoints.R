#(x,y)中心点坐标,interval坐标间隔,有边界
#requireNamespace('data.table')
SearchPoints <- function(x=0,
                         y=0,
                         SearchPoints0,
                         xmin=-1000,
                         xmax=1000,
                         ymin=-1000,
                         ymax=1000){
  #require(data.table)
  PointsPosition <- data.table(SearchPoints0+
    matrix(rep(c(x,y),each=nrow(SearchPoints0)),ncol = 2)) 
  # PointsPosition <- SearchPoints0
  # PointsPosition <- data.table(x=PointsPosition$x+x,y=PointsPosition$y+y)
  # PointsPosition <- data.frame(x=PointsPosition$x+x,y=PointsPosition$y+y)
  # PointsPosition <- PointsPosition[x<=xmax&x>=xmin&y<=ymax&y>=ymin,]
  PointsPosition <- subset(PointsPosition,xmin<=x&x<=xmax&ymin<=y&y<=ymax) # 边界处理
  rownames(PointsPosition) <- c(1:nrow(PointsPosition))
  return(PointsPosition)
}
