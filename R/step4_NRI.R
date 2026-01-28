library(dplyr)
library(glmnet)
library(caret)
library(pROC)
library(readxl)


#download the data sets from step2
file_list<-list.files(path="...", pattern = "CDL_", all.files = FALSE,
                      full.names = T, recursive = FALSE,
                      ignore.case = F, include.dirs = T, no.. = FALSE)
#download the data sets based including the combination information (I have given an example.)
merged2<-read_excel("Comb_State.xlsx")

for(i in 1:nrow(merged2)){
  print(i)
  eco1<-sprintf("%02d", as.numeric(as.character(merged2[i,1])))
  mydat<-read.csv(file_list[sub("_range_.*", "",sub(".*_eco_", "", file_list))==eco1])
  eco2<-sprintf("%02d", as.numeric(as.character(merged2[i,2])))
  exdata1<-read.csv(file_list[sub("_range.*", "",sub(".*_eco_", "", file_list))==eco2])
  #combine the data sets from two Eco region
  exdata <- bind_rows(mydat, exdata1)
  exdata[is.na(exdata)]<-0
  #build the model
  x_mat<-as.matrix(exdata[,colnames(exdata)[substr(colnames(exdata),1,3)=="cdl"]])
  myglmnet<-cv.glmnet(x=x_mat, y=exdata$range, family="binomial")
  #save the model
  save(myglmnet, file=paste0("Model_range_ecoreg_",eco1,".RData"))
  
}


