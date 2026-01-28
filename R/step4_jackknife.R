
library(MASS)
library(dplyr)
library(nleqslv)
#read the data sets from step3_jackknife
file_list<-list.files(pattern = "...", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)


result_mat<-NULL

state_list<-unique(sub(".*_", "",sub("_group.*", "", file_list)))
#match the CDL estimates with NRI released estimates
for(state in state_list){
  print(state) 
  file_list_tmp<-file_list[sub(".*_", "",sub("_group.*", "", file_list))==as.character(state)]
  file_list_tmp <- file_list_tmp[order(as.numeric(sub(".*_(\\d+).csv", "\\1", file_list_tmp)))]

  for(i in 1:length(file_list_tmp)){
  mydat5<-read.csv(file_list_tmp[i])
  mydat5[is.na(mydat5)]<-0
  group<-sub(".*group_","",sub(".csv","",file_list_tmp[i]))
  state_num<-code[code$abbr==as.character(state),]
  state_num<-sprintf("%02d", as.numeric(as.character(state_num$code)))

  group<-as.numeric(group)
  T_uf<-NRI[NRI$State==as.numeric(state_num),group+2]

  f <- function(Y, Xi, Tau) {
    sum((exp(Y + Xi)/(1+exp(Y + Xi)))*mydat5$n_pixel*(1-mydat5$P2017)*0.2223946) - Tau
  }
  
  initial_Yi <- 0
  
  gamma <- nleqslv(x = initial_Yi, fn = f, Xi = mydat5$X_value, Tau = T_uf)
  gamma<-gamma$x

  mydat5$p_hat_new<- exp(gamma + mydat5$X_value) / (1 + exp(gamma + mydat5$X_value))
  
  result_mat<-rbind(result_mat, c(state, group, gamma, T_uf, sum(mydat5$p_hat_new*mydat5$n_pixel*(1-mydat5$P2017))*0.2223946))

}

}

colnames(result_mat)<-c("state", "group", "gamma", "NRI", "CDL_match")









