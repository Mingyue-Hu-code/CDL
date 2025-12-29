
library(MASS)
library(dplyr)
#read the data sets that include the p_hat
file_list<-list.files(pattern = "with", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)

state_list<-unique(sub(".*_", "",sub("_eco.*", "", file_list)))

for(state in state_list){
  print(state)
  file_list_tmp<-file_list[sub(".*_", "",sub("_eco.*", "", file_list))==as.character(state)]
  for(i in 1:length(file_list_tmp)){
  mydat<-read.csv(file_list_tmp[i])
  eco<-sub("_.*", "",sub(".*_eco_", "", file_list_tmp[i]))
  #add the eco indicate for type 2 data sets
  mydat$eco<-sub("_.*", "",sub(".*_eco_", "", file_list_tmp[i]))
  write.csv(mydat, file=paste0("state_", state, "_eco_", eco, "_with_phat_new.csv"), row.names = F)
  }
  #combine the data sets within one state
  mydat5<-bind_rows(lapply(file_list_tmp, read.csv))
  write.csv(mydat5, file=paste0("/vol/data/zhuz/dgjang/CDL/range_Mingyue/BLM_eco/state_level/state_range_",state,".csv"), row.names = F)

}

