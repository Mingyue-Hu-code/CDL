library(MASS)
library(dplyr)
library(glmnet)
library(caret)
library(pROC)
library(coefplot)
library(fastglm)
library(lattice)
library(stringr)

#download the NRI datasets generate by Tracy
file_list<-list.files(path="Q:/projects/nri/annual-update/CDL_Analysis_Files_2023_Fall", pattern = "7.csv", all.files = FALSE,
                      full.names = T, recursive = FALSE,
                      ignore.case = F, include.dirs = T, no.. = FALSE)
#add the State code to the datasets
state_list<-read.csv("state_level_est.csv")
colnames(state_list)<-"abbr"
code<-read.csv("State_code.csv")
state_list<-merge(state_list, code, by = "abbr", all.x = TRUE)
state_list<-sprintf("%02d", state_list$code)
#download the datasets with Eco region indicator
file_list1<-list.files(path="Q:/projects/nri/annual-update/CDL_Analysis_Files_2023_Fall/ecoreg_lvl3_2024", pattern = "vgis", all.files = FALSE,
                      full.names = T, recursive = FALSE,
                      ignore.case = F, include.dirs = T, no.. = FALSE)


for(state in state_list){
  print(state)
  file_list_tmp<-file_list[sub("_.*", "",sub(".*_pt_", "", file_list))==state]
  exdata<-bind_rows(lapply(file_list_tmp, read.csv))
  
  file_list_tmp1<-file_list1[sub("_.*", "",sub(".*_pt_", "", file_list1))==state]
  exdata1<-bind_rows(lapply(file_list_tmp1, read.csv))
  exdata1<- select(exdata1, "point_id", "us_l3code")
  #combine the datasets with Eco region indicator
  exdata2<-merge(exdata,exdata1,by = "point_id", all.x = TRUE)
  #save the datasets with Eco region indicator
  region_vec<-setdiff(unique(exdata2$us_l3code), NA)
  for(k in region_vec){
    print(k)
    k1<-sprintf("%02d",k)
    write.csv(exdata2[which(exdata2$us_l3code==k),], file=paste0("ecor_", k1,"_state_",state,".csv"), row.names=F)
  }
  
  write.csv(exdata2[is.na(exdata2$us_l3code),], file=paste0("ecor_NA_state_",state,".csv"), row.names=F)
}  

#download new version data sets generate by Tracy
file_list<-list.files(path="Q:/projects/nri/annual-update/CDL_Analysis_Files_2023_Fall", pattern = "7_rv1.csv", all.files = FALSE,
                      full.names = T, recursive = FALSE,
                      ignore.case = F, include.dirs = T, no.. = FALSE)
New_list<-unique(sub(".*_pt_([0-9][0-9])_cdl_.*","\\1",file_list))

state_list<-read.csv("state_level_est.csv")
colnames(state_list)<-"abbr"
code<-read.csv("State_code.csv")
state_list<-merge(state_list, code, by = "abbr", all.x = TRUE)
state_list<-sprintf("%02d", state_list$code)

state_list1<-intersect(New_list,state_list)
#with Eco region
file_list1<-list.files(path="Q:/projects/nri/annual-update/CDL_Analysis_Files_2023_Fall/ecoreg_lvl3_2024", pattern = "vgis", all.files = FALSE,
                       full.names = T, recursive = FALSE,
                       ignore.case = F, include.dirs = T, no.. = FALSE)

#update the data sets
for(state in state_list1){
  print(state)
  file_list_tmp<-file_list[sub("_.*", "",sub(".*_pt_", "", file_list))==state]
  exdata<-bind_rows(lapply(file_list_tmp, read.csv))
  
  file_list_tmp1<-file_list1[sub("_.*", "",sub(".*_pt_", "", file_list1))==state]
  exdata1<-bind_rows(lapply(file_list_tmp1, read.csv))
  exdata1<- select(exdata1, "point_id", "us_l3code")
  
  exdata2<-merge(exdata,exdata1,by = "point_id", all.x = TRUE)
  
  region_vec<-setdiff(unique(exdata2$us_l3code), NA)
  for(k in region_vec){
    print(k)
    k1<-sprintf("%02d",k)
    write.csv(exdata2[which(exdata2$us_l3code==k),], file=paste0("ecor_", k1,"_state_",state,".csv"), row.names=F)
  }
  
  write.csv(exdata2[is.na(exdata2$us_l3code),], file=paste0("ecor_NA_state_",state,".csv"), row.names=F)
}  


