##set up library====
packs <- list("tidyverse",'doParallel','foreach','stringr')
lapply(packs, require, character.only = T)
# Main directory where all the sub-dir with files are stored ====
common_path = "D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc"
# set up wd
setwd(common_path)

# inital records
### Read occurrence files of 51620 species from sub-directories###
files_to_read = list.files(
  path = common_path,        # directory to search within
  pattern = ".*(NonUnique).*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
#sum(str_detect(files_to_read,"/occ"))

# note this dataset is big  
# number of tree species:51620
data_lst = lapply(files_to_read, read.csv) 
occ <- data.table::rbindlist(data_lst,fill=TRUE)
occ2 = unique(occ)
# Main directory where all the sub-dir with files are stored ====
common_path = "D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc"
# set up wd
setwd(common_path)

# After clean
### Read occurrence files of cleaned species from sub-directories###
files_to_read2= list.files(
  path = common_path,        # directory to search within
  pattern = ".*^(occ).*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)


# Main directory where all the sub-dir with files are stored ====
common_path = "D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange/"
# set up wd
setwd(common_path)

# After clean
### Read alpha hull files of cleaned species from sub-directories###
files_to_read3= list.files(
  path = common_path,        # directory to search within
  pattern = ".*_alphaInfo..*rds$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
data_lst = lapply(files_to_read3, readRDS)

# loop get alpha hull levels
alpha_toDo<-c()
a<-1
foreach(i = data_lst) %do% {
  alpha <- unlist(i)[2] 
  print(id)
  alpha_toDo[a]<-alpha
  a<-a+1
}
alpha <- noquote(alpha_toDo)
sum(alpha=="MCH") # 376
sum(is.na(alpha))
a <- summary(as.numeric(alpha[!alpha=="MCH"])) #mean 42.8


