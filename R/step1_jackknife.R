library(dplyr)
library(glmnet)
library(caret)
library(pROC)
#read the type 1 data sets
file_list<-list.files(pattern = "...")

geo$state<-sprintf("%02d", geo$state)
geo$county<-sprintf("%03d", geo$county)
geo <- geo %>%
  mutate(point_id = paste0(state, county, "_", PSU, point))
geo<-geo[,c(5,36)]
###############
for(eco in eco_list){
  print(eco)
  file_list_tmp<-file_list[sub("_.*", "",sub(".*eco_", "", file_list))==eco]
  exdata3<-bind_rows(lapply(file_list_tmp, read.csv))
  exdata3[is.na(exdata3)]<-0
  
  exdata<-merge(exdata3, geo, by = "point_id", all.x = TRUE)
  exdata$state <- substr(exdata$point_id, 1, 2)
  exdata <- exdata[order(exdata$state, exdata$geo), ]
  
  unique_geos <- unique(exdata$geo)
  groups <- vector("list", 29)
  for (i in 1:29) {
    groups[[i]] <- data.frame()
  }
  for (i in 1:length(unique_geos)) {
    geo_value <- unique_geos[i]
    
    # Find all rows with this 'Geo' value
    rows <- which(exdata$geo == geo_value)
    
    # Determine the group number (1 through 29) based on the unique value's position
    group_number <- ((i - 1) %% 29) + 1
    
    # Add all rows with the current 'Geo' value to the corresponding group
    groups[[group_number]] <- rbind(groups[[group_number]], exdata[rows, ])
  }
  models <- list()
  
  for (i in 1:29) {
    groups_to_include <- groups[-i]
    
    # build the model
    exdata1 <- do.call(rbind, groups_to_include)
    x_mat<-as.matrix(exdata1[,colnames(exdata1)[substr(colnames(exdata1),1,3)=="cdl"]])
    myglmnet<-cv.glmnet(x=x_mat, y=exdata1$range, family="binomial")
    
    # Save the model to the list
    models[[i]] <- myglmnet
  }
  
  
}

