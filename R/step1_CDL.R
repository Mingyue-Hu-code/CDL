
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
#download the model 
eco_list<-sub("...")
#read the CDL matrix data sets
file_list<-list.files(pattern = "...")

for(i in 1:length(file_list)){
  print(i)
  mydat5<-read.csv(file_list[i])
  mydat5[is.na(mydat5)]<-0
  eco<-sprintf("%02d", as.numeric(sub("_.*","",sub("ecor_","",file_list[i]))))
  state<-sub(".*_","",sub(".csv","",file_list[i]))
  #we don't consider the state FL here.
  #calculate the p_hat 
  if(state!="FL"){
  load(paste0("Range_ecoreg_",eco,".RData"))
  mydat5[,setdiff(row.names(myglmnet$glmnet.fit$beta), colnames(mydat5))]<-0
  range_dat<-mydat5[,c("range",row.names(myglmnet$glmnet.fit$beta))]
  x_mat<-as.matrix(mydat5[,row.names(myglmnet$glmnet.fit$beta)])
  mydat5$p_hat_lasso_min<-predict(myglmnet, newx=x_mat, s="lambda.min",type="response")
  #compute the X*Beta
  coef<-coef(myglmnet, s = "lambda.min")[-1,]
  mydat5$X_value<-t(t(coef)%*%t(x_mat))+coef(myglmnet, s = "lambda.min")[1,]


  write.csv(mydat5[,c("POINT_X_center", "POINT_Y_center", "P2017", colnames(mydat5)[substr(colnames(mydat5),1,6)=="county"], 
"delta", "n_pixel", "range", "p_hat_lasso_min", "X_value")], 
file=paste0("..."), row.names = F)
 
} 
}
