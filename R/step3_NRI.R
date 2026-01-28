library(dplyr)
library(glmnet)
library(caret)
library(pROC)
#download the data sets from step 2
file_list<-list.files(pattern = "...", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)


eco_list<-sort(sub(".*_","",sub("_range.*","",file_list)))


###############
for(eco in eco_list){
  print(eco)
  file_list_tmp<-file_list[sub("_.*", "",sub("CDL_eco_", "", file_list))==eco]
  exdata3<-bind_rows(lapply(file_list_tmp, read.csv))
  exdata3[is.na(exdata3)]<-0
  #build the model and find the trouble Ecoregion
  if(sum(exdata3$range)>2) {
    x_mat<-as.matrix(exdata3[,colnames(exdata3)[substr(colnames(exdata3),1,3)=="cdl"]])
    myglmnet <- cv.glmnet(x=x_mat, y=exdata3$range, family="binomial")
    save(myglmnet, file=paste0("Model_range_ecoreg_",eco,".RData"))
  }
  
  else if(sum(exdata3$range)<2|sum(exdata3$range)==2) {
    message("Warning: this eco has too small sample points!!!")
  }
}
###################combine




