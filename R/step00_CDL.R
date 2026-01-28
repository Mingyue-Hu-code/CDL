
library(MASS)
library(dplyr)
library(glmnet)
library(caret)
library(pROC)
library(coefplot)
library(fastglm)
library(lattice)
library(stringr)


file_list<-list.files(path="...")

state_list<-sub(".*_e_([0-9][0-9])_ecoreg_.*","\\1",file_list)

for(i in 1:length(file_list)){
  exdata<-read.csv(file_list[i])
  
  NRI_data<-exdata[exdata$seq_num==0,c("centroid_x", "centroid_y", "point_id", "range", "pasture", "seq_num", "ecoreg_lv3_code")]
  NRI_data$rangeland<-as.numeric(!is.na(NRI_data$range)&(NRI_data$range==1))
  NRI_data$pastureland<-as.numeric(!is.na(NRI_data$pasture)&(NRI_data$pasture==1))
  NRI_data$state=as.numeric(substr(NRI_data$point_id,1,2))
  NRI_data$county=as.numeric(substr(NRI_data$point_id,3,5))
  
  exdata$rangeland=as.integer(!is.na(exdata$range)&(exdata$range==1))
  
  
  name_vec<-sort(unique(exdata$cdl_2017))
  fun_list<-lapply(name_vec, function(y) as.formula(paste0("~mean(.x==", y,")")))
  names(fun_list)<-paste0("c", name_vec)
  filter_var<-7
  
  mydat<- NRI_data %>%
  group_by(point_id) %>%
    filter(seq_num<=filter_var^2-1) %>%
    summarize(
      n_pixel=n(),
      POINT_X_center=quantile(POINT_X, 0.5, type=1),
      POINT_Y_center=quantile(POINT_Y, 0.5, type=1),
      delta=max(complete.cases(pastureland)),
      pasture=max(pastureland, na.rm=T),
      
      # range1=sum(range==1),
      # range0=sum(range==0),
      
      across(cdl_2017, fun_list, .names="{.col}_{.fn}")
      
    )
  
  save(NRI_data, fun_list, file=paste0("CDL_NRI_",state_list[i],".RData"))
  rm(exdata, NRI_data, name_vec, fun_list)
}
