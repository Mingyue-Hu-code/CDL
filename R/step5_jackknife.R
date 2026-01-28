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
#read the data sets that include the adjusted p_hat
file_list<-list.files(pattern = "...", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)
result_mat1<-NULL

state_list<-unique(sub(".*_", "",sub("_group.*", "", file_list)))
#compute the county level total area
for(state in state_list){
  print(state) 
  file_list_tmp<-file_list[sub(".*_", "",sub("_group.*", "", file_list))==as.character(state)]
  result_mat<-NULL

  for(i in 1:length(file_list_tmp)){
  mydat5<-read.csv(file_list_tmp[i])
  mydat5[is.na(mydat5)]<-0
  state<-sub("_group.*", "",sub("state1_", "", file_list_tmp[i]))
  group<-sub("_with.*", "",sub(".*group_", "", file_list_tmp[i]))
  county<-unique(mydat5[,colnames(mydat5)[substr(colnames(mydat5),1,6)=="county"][1]])
  mydat5 <- mydat5 %>%
    mutate(ind = pmax((1-P2017), blm))
  
for(j in county){
  mydat6<-mydat5[mydat5[,colnames(mydat5)[substr(colnames(mydat5),1,6)=="county"][1]]==j,]
  result_mat<-rbind(result_mat,c(state, j, group, sum(mydat6$p_hat_new*mydat6$n_pixel*mydat6$ind)*0.2223946))
}
}

  colnames(result_mat)<-c("state", "county", "group", "area")
  result_mat1<-rbind(result_mat1,result_mat)
}

colnames(result_mat1)<-c("state", "county", "group", "area")

