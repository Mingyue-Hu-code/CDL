
library(MASS)
library(dplyr)
library(glmnet)
library(caret)
library(pROC)
library(coefplot)
library(fastglm)
library(lattice)
library(stringr)

########process the uncleaned data from step1
file_list<-list.files(path="...", pattern = "ecor_", all.files = FALSE,
                      full.names = T, recursive = FALSE,
                      ignore.case = F, include.dirs = T, no.. = FALSE)

eco_list<-unique(sub("_state_.*","",sub(".*ecor_","",file_list)))


###################To get 7*7 size
for(i in 1:length(eco_list)){
  print(i)
  file_list_tmp<-file_list[sub(".*ecor_", "",sub("_state_.*", "", file_list))==eco_list[i]]
  #read the data from same Eco region
  exdata<-bind_rows(lapply(file_list_tmp, read.csv))
  exdata[is.na(exdata)]<-0
  #creat pasture indicator
  exdata$pasture <- ifelse(exdata$bu_code == 3, 1, 0)
  
  NRI_data<-exdata[,c("centroid_x", "centroid_y", "point_id", "range", "pasture", "cdl_2017", "seq_num", "us_l3code")]
  NRI_data$rangeland<-as.numeric(!is.na(NRI_data$range)&(NRI_data$range==1))
  NRI_data$pastureland<-as.numeric(!is.na(NRI_data$pasture)&(NRI_data$pasture==1))
  NRI_data$state=as.numeric(substr(NRI_data$point_id,1,2))
  NRI_data$county=as.numeric(substr(NRI_data$point_id,3,5))
  #To get 7*7 size
  filter_var<-7
  name_vec<-sort(unique(NRI_data$cdl_2017))
  fun_list<-lapply(name_vec, function(y) as.formula(paste0("~mean(.x==", y,")")))
  names(fun_list)<-paste0("c", name_vec)
  #To get number of pixels, delta, pasture/range indicator, X-variable.
  mydat<- NRI_data %>%
    group_by(point_id) %>%
    filter(seq_num<=(filter_var^2-1)) %>%
    summarize(
      n_pixel=n(),
      POINT_X_center=quantile(centroid_x, 0.5, type=1),
      POINT_Y_center=quantile(centroid_y, 0.5, type=1),
      delta=max(complete.cases(rangeland)),
      pasture=max(pastureland, na.rm=T),
      range=max(rangeland, na.rm=T),
      across(cdl_2017, fun_list, .names="{.col}_{.fn}")
    )
  
  
  mydat$pasture[is.infinite(mydat$pasture)]<-0
  mydat$range[is.infinite(mydat$range)]<-0
  
  
  mydat4<-NULL
  mydat4<-cbind(mydat[,c("point_id", "POINT_X_center", "POINT_Y_center", "delta", "range", "pasture", "n_pixel", colnames(mydat)[substr(colnames(mydat),1,3)=="cdl"])])
  write.csv(mydat4, file=paste0("CDL_eco_",eco_list[i],"_range_7s.csv"), row.names=F)

  rm(list=c("mydat"))
  gc()
}                                        
####################

