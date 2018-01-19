require(EBImage)
require(SSLKE)
require(data.table)

if(!file.exists('.//demo//MISE.txt'))  file.create('.//demo//MISE.txt')

system.time(
  for (INDEX in 1:5){
    print(INDEX)
    rm(list=ls())
    gc()
    source('.//demo//example7_cv.R')
    write.table(data.frame(MISE,MISEe),file = './/demo//MISE.txt',append = TRUE,row.names = FALSE,col.names = FALSE)
  }
)
