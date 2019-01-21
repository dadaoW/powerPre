
load("./hstr.RData")

DataUpdate <- function(data_source_path=FALSE,newdata_path=FALSE,dwnld_data = FALSE
                       ,save_rdata = FALSE){
  #数据更新部分---start。
  print("dataupdate start!")
  if(newdata_path != FALSE & (exists("data_1022")) ){
    newdata <- read.csv(newdata_path,header = T,fileEncoding = 'GBK')
    data_1022 <- rbind(data_1022,newdata)
    data_1022 <- data_1022[duplicated(data_1022),]#避免重复update造成的数据重复。
    if(dwnld_data != FALSE){#最新数据下载到本地。
      filename <- paste0("data_updated_",substring(Sys.time(),1,10),".csv")
      write.csv(data_1022,filename)
    }
  }
  if(newdata_path == FALSE & data_source_path != FALSE & !(exists("data_1022")) ){
    #newdata <- read.csv(newdata_path,header = T,fileEncoding = 'GBK')
    data_1022 <- read.csv(data_source_path,header = T,fileEncoding = 'GBK')
    #data_1022 <- rbind(data_1022,newdata)
    data_1022 <- data_1022[!duplicated(data_1022),]#避免重复update造成的数据重复。
    if(dwnld_data != FALSE){#最新数据下载到本地。
      filename <- paste0("data_updated_",substring(Sys.time(),1,10),".csv")
      write.csv(data_1022,filename)
    }
  }
  assign("data_1022",data_1022,envir=.GlobalEnv)#以备复用。
  print("dataupdate finish!")
  #return(list(data_1022 = data_1022))
  #数据更新部分---end。
  if(save_rdata != FALSE){
    save.image("./hstr.RData")
    print(file.mtime("./hstr.RData"))
  }
}
