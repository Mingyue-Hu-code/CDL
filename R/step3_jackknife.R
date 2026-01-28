
library(MASS)
library(dplyr)
#read the data sets from step2_jackknife
file_list<-list.files(pattern = "group", all.files = FALSE,
                        full.names = F, recursive = FALSE,
                        ignore.case = F, include.dirs = T, no.. = FALSE)

state_list<-unique(sub(".*_", "",sub("_eco.*", "", file_list)))
#combine the data sets within one state
for(state in state_list){
  print(state)
  file_list_tmp<-file_list[sub(".*_", "",sub("_eco.*", "", file_list))==as.character(state)]
  group_list<-unique(sub(".*_", "",sub(".csv", "", file_list_tmp)))
for(group in group_list){
  file_list_tmp1<-file_list_tmp[sub(".*_", "",sub(".csv", "", file_list_tmp))==group]
for (i in 1:length(file_list_tmp1)){
  mydat5<-read.csv(file_list_tmp1[i])  
  eco<-sub("_.*", "",sub(".*_eco_", "", file_list_tmp1[i]))
  mydat5$eco<-sub("_.*", "",sub(".*_eco_", "", file_list_tmp1[i]))
  write.csv(mydat5, file=paste0("state1_", state, "_eco_", eco, "_group_", group, ".csv"), row.names = F)
}
  mydat<-bind_rows(lapply(file_list_tmp1, read.csv))  
  write.csv(mydat, file=paste0("/vol/data/zhuz/dgjang/CDL/range_Mingyue/BLM_eco/group/state_level/state_range_",state,"_group_", group, ".csv"), row.names = F)

}
}

