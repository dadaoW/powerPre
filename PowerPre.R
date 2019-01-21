load("./hstr2.RData")

PowerPre <- function(fpd = FALSE,reload_channels=FALSE,yewei_avg=FALSE
                     ,yewei_14=FALSE,yewei_delta=FALSE
                     ,dwnld_power = FALSE,save_rdata = FALSE){
  #fpd = "5068.7-2"
  #reload_channels="D15,F05,L15,Q10,S17"
  #yewei_avg=FALSE
  #yewei_14=FALSE
  #yewei_delta=FALSE
  ##dwnld_data = FALSE
  #dwnld_power = TRUE
  #save_rdata = FALSE
  #模型复用部分---start。
  #reload_channels形式为“A09,A10,A11......”
  #test---start---
  #print("part1")
  #print(exists("rf_A13"))
  #print(length(ls()[substring(ls(),1,3) == "rf_" & nchar(ls()) == 6]) == 2)
  ##ls(pattern="rf_")
  #print(ls())
  #print(channel_total)
  #rf = get(ls()["rf_A13" == ls()])
  #print(rf)
  #test---end---
  #if(length(ls()[substring(ls(),1,3) == "rf_" & nchar(ls()) == 6]) == 2){
  #  print("part2")
    

  library(randomForest)
  m_test <- matrix(data = 0,nrow = 1,ncol = 380)
  colnames(m_test) <- channel_total
  reload_channels <- unlist(strsplit(reload_channels,","))
  #colnames(m_test) == reload_channels
  for (i in 1:length(reload_channels)) {
    m_test[,which(colnames(m_test)==reload_channels[i])] <- 1
  }
  colnames(m_test) <- paste0(colnames(m_test),".5")
  
  reload_and_unreload_channels <- m_test#reload_channels对应的值变为1，其他的channels对应的值为0
  data_input <- cbind(data_1217_train[nrow(data_1217_train),1:410]
                      ,reload_and_unreload_channels
                      ,data_1217_train[nrow(data_1217_train),791:1170])
  
  channel_matrix <- matrix(data = 0,ncol = 22,nrow = 22)#通道分布矩阵
  colnames(channel_matrix) <- (1:22)#通道分布列名
  rownames(channel_matrix) <- LETTERS[seq(1,23)][-9]#通道分布行名
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
  start_info <<- "Here is forecasting"
  print(start_info)
  done <<- NULL
  for (i in c(1:380)) {
    channel_name <- unlist(strsplit(channel_list[i],"[.]"))[1]
    channel_m_r <- unlist(strsplit(channel_list[i],""))[1]
    channel_m_c <- as.numeric(paste(unlist(strsplit(channel_list[i],""))[2]
                                    ,unlist(strsplit(channel_list[i],""))[3],sep=""))
    rf_name <- paste0("rf_",channel_name)
    #rf = get(ls()[rf_name == ls()])#得到某通道的模型
    if(exists(rf_name)){
    rf = get(rf_name)
    reload_train <- apply(f(channel_matrix,channel_m_c,channel_m_r,1,0,data_input[,((411:790))])
                          ,FUN = sum,MARGIN = 1)*2 +
      apply(f(channel_matrix,channel_m_c,channel_m_r,2,1,data_input[,((411:790))])
            ,FUN = sum,MARGIN = 1)*1 +
      f(channel_matrix,channel_m_c,channel_m_r,0,0,data_input[,((411:790))])*10
    
    model_input  <-  cbind(data_input[,c(2:30,i+30,(411:790),i+790)]
                           ,reload_train)#[reload_train < 10,]
    model_target <- predict(rf,model_input)
    power <- model_target+data_input[,i+790]
    channel_matrix[channel_m_r,channel_m_c] <- round(power)
    done <<- c(done,channel_name)
    #print(done)
    }
    else{channel_matrix[channel_m_r,channel_m_c] <- "no_model"}
    assign("power_distribution_matrix",channel_matrix,envir=.GlobalEnv)
  }

  end_info <<- "The power_distribution_matrix has finished!!!"
  print(end_info)
  
  if(dwnld_power != FALSE){
    filename <- paste0("power_distribution_",fpd,"-",substring(Sys.time(),1,10),".csv")
    write.csv(channel_matrix,filename)
    print("Write power-distribution-matrix successfully!")
  }
#模型复用部分---end。
  if(save_rdata != FALSE){
    save.image("./hstr2.RData")
  }
  #}
}
