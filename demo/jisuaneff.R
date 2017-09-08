x <- runif(1e8)
Rprof("Rprof.out")
y <- sampvar(x)
Rprof(NULL)
summaryRprof("Rprof.out")

Rprof(tmp <- tempfile())
example(glm)
Rprof()
summaryRprof(tmp)
unlink(tmp)

install.packages("proftools")
source("http://bioconductor.org/biocLite.R")
biocLite(c("graph", "Rgraphviz"))
library(proftools)
p <- readProfileData(filename="Rprof.out")
plotProfileCallGraph(p, style=google.style, score="total")


Rprof("Rprof-mem.out", memory.profiling=TRUE)
y <- sampvar(x)
Rprof(NULL)
summaryRprof("Rprof-mem.out", memory="both")

print(object.size(x), units="auto")

gcinfo(TRUE)
y <- sampvar(x)
gcinfo(FALSE)


gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x, i)
gcinfo(verbose = FALSE) #-- don't show it anymore

gc(TRUE)

gc(reset = TRUE)



library(gputools)
A <- lapply(c(1:5), function(x) {
  matrix(rnorm((x*1e2) * 1e2), 1e2, (x*1e2))})
cpu_k_time <- sapply(A, function(x) {
  system.time(cor(x=x, method="kendall"))[[3]]})
gpu_k_time <- sapply(A, function(x) {
  system.time(gpuCor(x=x, method="kendall"))[[3]]})
K <- data.frame(cpu=cpu_k_time, gpu=gpu_k_time)


library(compiler)
LKE_side.compiled3 <- cmpfun(LKE_side, options=list(optimize=3))

time1 <- Sys.time()
hat_Z <- data.frame(matrix(numeric(0),ncol=6,nrow = nrow(Test)))
Test <- as.data.frame(Test)
SearchPoints0 <- SearchPoints0(h=5)
for (i in 1:nrow(Test)){
  #print(i)
  hat_Z[i,] <- LKE_side (m=Test[i,1],n=Test[i,2],h=5,
                        data=Test,
                        SearchPoints0,
                        xmin = 0,
                        xmax = 100,
                        ymin = 0,
                        ymax = 100,
                        scale=100)
}#论文中单边的局部线性核光滑
Sys.time()-time1
