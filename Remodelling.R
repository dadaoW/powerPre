load("./hstr.RData")
Remodelling <- function(local_data = FALSE,local_path = FALSE
                        ,save_rdata = FALSE){
  if(local_data == TRUE){
    data_1022 <- read.csv(local_path,header = T,fileEncoding = 'GBK')
  }
  #模型训练部分---start。
  #data_1022 <- read.csv(data_source_path,header = T,fileEncoding = 'GBK')
  #nrow(data_1022)
  #data_1022 <- dataupdate$data_1022
  data_1022_fpd <- data_1022[,1]#满功率日。
  #data_1022_part1 <- data_1022[,2]#硼浓度，存在硼浓度为NA的情况。
  data_1022_part1_2 <- data_1022[,3]#平均液位delta。
  data_1022_part1_3 <- data_1022[,c(4:17)]#14个区域的初始液位。
  data_1022_part1_4 <- data_1022[,c(18:31)]#14个区域的液位偏差。
  #data_1022_part2 <- data_1022[,32:(32+380-1)]#反应增益。
  data_1022_part3 <- data_1022[,(32+380):(32+380+380-1)]#平均出口卸料燃耗。
  #data_1022_part4 <- data_1022[,(32+380+380):(32+380+380+380-1)]#最大棒束功率。
  #data_1022_part5 <- data_1022[,(32+380+380+380):(32+380+380+380+380-1)]#通道驻留时间。
  data_1022_part6 <- data_1022[,(32+380+380+380+380):(32+380+380+380+380+380-1)]#换料时间步长。
  #data_1022_part7 <- data_1022[,(32+380+380+380+380+380):(32+380+380+380+380+380+380-1)]#通道卸料燃耗。
  data_1022_part8 <- data_1022[,(32+380+380+380+380+380+380):(32+380+380+380+380+380+380+380-1)]#通道功率。
  data_1022_part9 <- data_1022[,(32+380+380+380+380+380+380+380):(32+380+380+380+380+380+380+380+380-1)]#nextpower。
  data_1022_part6[data_1022_part6 > 0] = 1# to 0/1
  #data_1022_part7[data_1022_part7 > 0] = 1# to 0/1
  
  delta <- data_1022_part9 - data_1022_part8#从本满功率日到下一满功率日的变化值的相反数。
  data_1217_use <- cbind(data_1022_fpd,#1
                         data_1022_part1_2,#1
                         data_1022_part1_3,#14
                         data_1022_part1_4,#14
                         #data_1022_part2,
                         data_1022_part3,#380
                         #data_1022_part4,
                         #data_1022_part5,
                         data_1022_part6,#380
                         #data_1022_part7,
                         data_1022_part8,#380
                         delta#data_1022_part9
  )
  data_1217_use <- data_1217_use[c(-1:-205),]#csv中去掉<=500满功率天的样本。
  #data_1217_use <- data_1217_use[c(-1:-205,-1867:-nrow(data_1217_use)),]
  data_1217_use <- na.omit(data_1217_use)#删掉所需字段中含有NA的
  #set.seed(12345)#随机种子。
  #data_1217_use <- data_1217_use[order(runif(nrow(data_1217_use))),]#随机化处理
  data_1217_train <- data_1217_use[1:nrow(data_1217_use),]#100%作为训练集。
  assign("data_1217_train",data_1217_train,envir=.GlobalEnv)
  #data_1217_test <- data_1217_use[ceiling(nrow(data_1217_use)*0.9):nrow(data_1217_use),]#10%作为测试集。

  channel_matrix <- matrix(data = 0,ncol = 22,nrow = 22)#通道分布矩阵
  colnames(channel_matrix) <- (1:22)#通道分布列名
  rownames(channel_matrix) <- LETTERS[seq(1,23)][-9]#通道分布行名
  channel_matrix_max <- channel_matrix#用于模型构建过程中最大偏差统计。
  
  channel_list <- colnames(data_1217_train)[791:1170]#得到channel_list
  oushu <- seq(1,760,2)#偶数数列
  channel_total <- unlist(strsplit(channel_list,"[.]"))[oushu]#得到channel_total
  assign("channel_total",channel_total,envir=.GlobalEnv)
  assign("channel_list",channel_list,envir=.GlobalEnv)
  #write.csv(channel_total,"channel_total.csv")
  #下面函数f是用来追踪通道周边是否有换料通道，并且汇总周边通道换料对该通道影响的方法。
  f <- function(channel_matrix,channel_m_c,channel_m_r,around_out,around_in,data){
    zimu <- rownames(channel_matrix)
    shuzi <- colnames(channel_matrix)
    index0 <- which(zimu == channel_m_r)
    index_u <- which(zimu == channel_m_r)-around_out
    if(index_u<1){index_u = 1}
    index_b <- which(zimu == channel_m_r)+around_out
    if(index_b>22){index_b = 22}
    shu <- zimu[index_u:index_b]
    index_l <- which(shuzi == channel_m_c)-around_out
    if(index_l<1){index_l = 1}
    index_r <- which(shuzi == channel_m_c)+around_out
    if(index_r>22){index_r = 22}
    heng <- shuzi[index_l:index_r]
    heng <- substr(as.character(as.numeric(heng)+100),2,3)
    total_combn <- combn(c(shu,heng),2)
    ch <- paste0(total_combn[1,which(total_combn[1,] %in% shu &
                                       total_combn[2,] %in% heng)],
                 total_combn[2,which(total_combn[1,] %in% shu &
                                       total_combn[2,] %in% heng)],".5")
    
    index_u_in <- which(zimu == channel_m_r)-around_in
    index_b_in <- which(zimu == channel_m_r)+around_in
    shu_in <- zimu[index_u_in:index_b_in]
    index_l_in <- which(shuzi == channel_m_c)-around_in
    index_r_in <- which(shuzi == channel_m_c)+around_in
    heng_in <- shuzi[index_l_in:index_r_in]
    heng_in <- substr(as.character(as.numeric(heng_in)+100),2,3)
    total_combn_in <- combn(c(shu_in,heng_in),2)
    ch_in <- paste0(total_combn_in[1,which(total_combn_in[1,] %in% shu_in &
                                             total_combn_in[2,] %in% heng_in)],
                    total_combn_in[2,which(total_combn_in[1,] %in% shu_in &
                                             total_combn_in[2,] %in% heng_in)],".5")
    
    if(length(ch_in) == length(ch)){ch_need <-ch}else{ch_need <- setdiff(ch,ch_in)}
    
    
    ch_need <- intersect(ch_need,paste0(channel_total,".5"))
    reload <- data[,ch_need]
    return(reload)
  }
  #追踪周边通道，[自定义],里面会用到对象channel_total。
  library(randomForest)
  for (i in 1:30) {
  channel_name <- unlist(strsplit(channel_list[i],"[.]"))[1]
  rf_name <- paste0("rf_",channel_name)
  
  channel_m_r <- unlist(strsplit(channel_list[i],""))[1]
  channel_m_c <- as.numeric(paste(unlist(strsplit(channel_list[i],""))[2]
                                  ,unlist(strsplit(channel_list[i],""))[3],sep=""))
  reload_train <- apply(f(channel_matrix,channel_m_c,channel_m_r,1,0,data_1217_train[,((411:790))])
                        ,FUN = sum,MARGIN = 1)*2 +
    apply(f(channel_matrix,channel_m_c,channel_m_r,2,1,data_1217_train[,((411:790))])
          ,FUN = sum,MARGIN = 1)*1 +
    f(channel_matrix,channel_m_c,channel_m_r,0,0,data_1217_train[,((411:790))])*10
  
  train_input = cbind(data_1217_train[,c(2:30,i+30,(411:790),i+790)],reload_train)
  train_target = data_1217_train[,(i+1170)]

  rf <- randomForest(train_input,train_target,type = "regression",ntree = 500)
  assign(rf_name,rf,envir=.GlobalEnv)#将模型储存到环境中，以备复用。
  print(paste0(rf_name," model has finished successfully!"))
#  if(pre_ornot == TRUE){
#    reload_train <- apply(f(channel_matrix,channel_m_c,channel_m_r,1,0,data_1217_test[,((411:790))])
#                          ,FUN = sum,MARGIN = 1)*2 +
#      apply(f(channel_matrix,channel_m_c,channel_m_r,2,1,data_1217_test[,((411:790))])
#            ,FUN = sum,MARGIN = 1)*1 +
#      f(channel_matrix,channel_m_c,channel_m_r,0,0,data_1217_test[,((411:790))])*10
#    test_input  <-  cbind(data_1217_test[,c(2:30,i+30,(411:790),i+790)]
#                          ,reload_train)[reload_train < 10,]
#    p <- predict(rf,test_input)
#    return(p)
#  }
  }
#模型训练部分---end。
  #return(list(channel_list = channel_list,channel_total=channel_total))
  if(save_rdata != FALSE){
    save.image("./hstr.RData")
    save.image("./hstr2.RData")
    print(file.mtime("./hstr.RData"))
  }
}
