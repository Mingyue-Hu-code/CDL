library(MASS)
library(dplyr)
library(glmnet)
library(lattice)
library(stringr)
library(ggplot2)
library(bigmemory)
library(caret)
library(pROC)
library(coefplot)
library(fastglm)
#read the data sets with adjusted p_hat
file_list<-list.files(pattern = "_with_phat_", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)
result_mat1<-NULL

for(i in 1:length(file_list)){ 
  print(i)
  mydat5<-read.csv(file_list[i])
  mydat5[is.na(mydat5)]<-0
  state<- sub("_with.*", "",sub("state1_", "", file_list[i]))
  county<-unique(mydat5[,colnames(mydat5)[substr(colnames(mydat5),1,6)=="county"][1]])
  #compute the BLM+non_federal indicator
  mydat5 <- mydat5 %>%
    mutate(ind = pmax((1-P2017), blm))
  #compute the county level total area
  result_mat<-NULL
for(i in county){
  mydat6<-mydat5[mydat5[,colnames(mydat5)[substr(colnames(mydat5),1,6)=="county"][1]]==i,]
  result_mat<-rbind(result_mat,c(state, i, sum(mydat6$p_hat_new*mydat6$n_pixel*mydat6$ind)*0.2223946))
}
  colnames(result_mat)<-c("state", "county", "area")
  write.csv(result_mat, file=paste0("/vol/data/zhuz/dgjang/CDL/range_Mingyue/BLM_eco/state_level/result_county_",state,".csv"), row.names = F) 
  result_mat1<-rbind(result_mat1,result_mat)
}

colnames(result_mat1)<-c("state", "county", "area")
write.csv(result_mat1, "result_county1.csv", row.names = F)

