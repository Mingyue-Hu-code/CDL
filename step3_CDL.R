
library(MASS)
library(dplyr)
library(nleqslv)
#read the data sets from step2_CDL
file_list<-list.files(pattern = "_range_", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)

code<-read.csv("State_code.csv")
#read the NRI released estimates
NRI<-read.csv("totals_states.csv")
result_mat<-NULL

for(i in 1:length(file_list)){
  print(i)
  mydat5<-read.csv(file_list[i])
  mydat5[is.na(mydat5)]<-0
  state<-sub(".*_","",sub(".csv","",file_list[i]))
  state_num<-code[code$abbr==as.character(state),]
  state_num<-sprintf("%02d", as.numeric(as.character(state_num$code)))
  #T_uf is the NRI estimate
  T_uf<-NRI[NRI$state==as.numeric(state_num),2]
  #solve the function to match the CDL estimate with NRI estimate
  f <- function(Y, Xi, Tau) {
    sum((exp(Y + Xi)/(1+exp(Y + Xi)))*mydat5$n_pixel*(1-mydat5$P2017)*0.2223946) - Tau
  }
  
  initial_Yi <- 0
  
  gamma <- nleqslv(x = initial_Yi, fn = f, Xi = mydat5$X_value, Tau = T_uf)
  gamma<-gamma$x
  #compute the adjusted p_hat
  mydat5$p_hat_new<- exp(gamma + mydat5$X_value) / (1 + exp(gamma + mydat5$X_value))
  #calculate the T_hat that should be equal to the NRI estimate
  result_mat<-rbind(result_mat, c(state, gamma, T_uf, sum(mydat5$p_hat_new*mydat5$n_pixel*(1-mydat5$P2017))*0.2223946))
  write.csv(mydat5, file=paste0("state1_", state,"_with_phat_match.csv"), row.names = F)
}


colnames(result_mat)<-c("state", "gamma", "NRI", "CDL_match")
write.csv(result_mat, "result_state_match.csv", row.names = F)












