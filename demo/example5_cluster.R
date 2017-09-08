require(data.table)
require(EBImage)
f1 <- function(x,y){
  z <- -2*(x-0.5)^2-2*(y-0.5)^2+sign((x-0.5)^2+(y-0.5)^2<0.25^2)
  return(z)
}
test <- data.table(x=rep(c(0:99),100),y=rep(c(0:99),each=100))
test$z <-(f1(test[["x"]]/100,test[["y"]]/100)+1)/2
#set.seed(111)
Test <- copy(test)
Test$z <- test[['z']]+rnorm(10000,0,0.2^2)

display(matrix(test[['z']],100))#原图

display(matrix(Test[['z']],100))#加噪


require(dplyr)
require(parallel)
require(foreach)
require(doParallel)
require(picture)


hat_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test)))
Test <- as.data.frame(Test)

bingx <- function(h,hat_Z,Test,test){
  require(picture)
  SearchPoints0 <- SearchPoints0(h)
  for (i in 1:nrow(Test)){
    hat_Z[i,] <- LKE_side(m=Test[i,1],n=Test[i,2],h=h,
                          data=Test,
                          SearchPoints0,
                          xmin = 0,
                          xmax = 100,
                          ymin = 0,
                          ymax = 100,
                          scale=100)
  }#论文中单边的局部线性核光滑
  result <- judge_u(data=hat_Z,u=0.0002599*4) #选择估计值
  return(MISE(result[,7],test[,3]))
}

#detectCores(logical = F) #确认核心数目 2

Rprof("Rprof-mem.out", memory.profiling=TRUE)
cl <- makeCluster(getOption('cl.cores', 2));
system.time({
   res <- parLapply(cl,4:7,bingx,hat_Z,Test,test)
})
stopCluster(cl)
Rprof(NULL)
summaryRprof("Rprof-mem.out", memory="both")



cl <- makeCluster(2)
registerDoParallel(cl)
system.time({
x <- foreach(x=4:7,.combine='rbind',.packages = 'picture') %dopar% bingx(x,hat_Z,Test,test)
})
stopCluster(cl)
