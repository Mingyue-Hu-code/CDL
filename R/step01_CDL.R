
library(MASS)
library(dplyr)
library(glmnet)
library(caret)
library(pROC)
library(coefplot)
library(fastglm)
library(lattice)
library(stringr)



NRI_data<-NRI_data %>% rename(POINT_X=centroid_x, POINT_Y=centroid_y) %>% dplyr::select(!c(point_id, pasture))

file_list<-list.files(pattern = "...")

mydat4<-NULL;

for(i in 1:length(file_list)){
  print(i)
  county<-as.numeric(sub(".csv", "",sub(".*pts_[0-9][0-9]","", file_list[i])))
  
  mydat<-read.csv(file_list[i], na.strings="")
  mydat<- mydat %>% rename(cdl_2017=grid_code)
  mydat$POINT_X<-round(mydat$POINT_X/3, -1)*3
  mydat$POINT_Y<-round(mydat$POINT_Y/3, -1)*3
  mydat<-base::merge(x=mydat, y=NRI_data[NRI_data$county==county,c("POINT_X", "POINT_Y", "rangeland")], by=c("POINT_X","POINT_Y"), all.x=T)
  
  
  seq_x<-seq(min(mydat$POINT_X), max(mydat$POINT_X), by=30*7)
  if(max(seq_x)!=max(mydat$POINT_X)) seq_x<-c(seq_x, max(mydat$POINT_X)+1) else seq_x[length(seq_x)]=max(mydat$POINT_X)+1
  seq_y<-seq(min(mydat$POINT_Y), max(mydat$POINT_Y), by=30*7)
  if(max(seq_y)!=max(mydat$POINT_Y)) seq_y<-c(seq_y, max(mydat$POINT_Y)+1) else seq_y[length(seq_y)]=max(mydat$POINT_Y)+1
  min_x<-min(mydat$POINT_X)
  max_x<-max(mydat$POINT_X)
  min_y<-min(mydat$POINT_Y)
  max_y<-max(mydat$POINT_Y)
  
  mydat<- mydat %>%
    mutate(section_x=(POINT_X-min_x)%/%(7*30)+1, 
           section_y=(POINT_Y-min_y)%/%(7*30)+1) %>% 
    group_by(section_x, section_y) %>% 
    summarize(
      n_pixel=n(),
      POINT_X_center=quantile(POINT_X, 0.5, type=1),
      POINT_Y_center=quantile(POINT_Y, 0.5, type=1),
      delta=max(complete.cases(rangeland)),
      range=max(rangeland, na.rm=T),    
      across(cdl_2017, fun_list, .names="{.col}_{.fn}")     
    )
  
  
  mydat$range[is.infinite(mydat$range)]<-0
  mydat$county=county
  
  print(head(mydat))
  mydat4<-rbind(mydat4, cbind(mydat[,c("POINT_X_center", "POINT_Y_center", "county", "delta", "range", "n_pixel", colnames(mydat)[substr(colnames(mydat),1,3)=="cdl"])]))
  
  
  rm(list=c("mydat"))
  gc()
}

####################
mydat4$POINT_X_center<-round(mydat4$POINT_X_center/21, -1)*21
mydat4$POINT_Y_center<-round(mydat4$POINT_Y_center/21, -1)*21

###################

feddat<-feddat<-read.csv("....") # filename should be changed
feddat$POINT_X_center<-round(feddat$POINT_X_center/21, -1)*21
feddat$POINT_Y_center<-round(feddat$POINT_Y_center/21, -1)*21
feddat<-feddat[!duplicated(feddat[,c("POINT_X_center", "POINT_Y_center")]), ]  %>% select(-county)

print(nrow(mydat4))
print(nrow(feddat))
mydat4<-merge(mydat4, feddat, by=c("POINT_X_center", "POINT_Y_center"), all.x=T)
print(nrow(mydat4))
mydat4 %>% rename(ECO_LV3=US_L3CODE) ->mydat4

region_vec<-setdiff(unique(mydat4$ECO_LV3), NA)


