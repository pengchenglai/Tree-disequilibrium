# Main directory where all the sub-dir with files are stored ====
common_path = "D:/quercus/Partage/Pengcheng/20211220"
# set up wd
setwd(common_path)
#set up library
packs <- list("tidyverse")
lapply(packs, require, character.only = T)
### Read occurrence files of 52168 species from sub-directories###
files_to_read = list.files(
  path = common_path,        # directory to search within
  pattern = ".*^(occ).*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
#sum(str_detect(files_to_read,"/occ"))

# 1.inital species list====
sporignal = strsplit(files_to_read,'/')
sporignal = lapply (sporignal, function (x) x[6])
sporignal = unlist (sporignal)
sporignal<- data.frame(species_name=sporignal)
write_csv (x = sporignal,
           file = paste0(common_path,'/original_species_names.csv'))
write_rds (sporignal,paste0(common_path,'/original_species_names.rds'))

# species list after data cleaning====
common_path1 = "D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc"
# set up wd
setwd(common_path1)
### Read occurrence files of 51620 species from sub-directories###
files_to_read1 = list.files(
  path = common_path1,        # directory to search within
  pattern = ".*(occStat).*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)

spClean = strsplit(files_to_read1,'/')
spClean = lapply (spClean, function (x) x[8])
spClean = unlist (spClean)
spClean<- data.frame(species_name=spClean,
                     data_cleaning_issues="No")# only without cleaning issue
write_csv (x = spClean,
           file = paste0(common_path1,'/cleaned_species_names.csv'))
write_rds (spClean,paste0(common_path1,'/cleaned_species_names.csv'))
spCleanfailed <- setdiff(sporignal$species_name,spClean$species_name)
spCleanfailed <- data.frame(species_name=spCleanfailed,
                            data_cleaning_issues="Yes")

# 2.species list with(out) cleaning issue(whole list)====
spclean <- rbind(spClean,spCleanfailed)

# orphaned hole issue for alpha-hull====
common_path2<- "D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/largeOcc"
# set up wd
setwd(common_path2)
### Read occurrence files of 107 species from sub-directories###
files_to_read2 = list.files(
  path = common_path2,        # directory to search within
  pattern = ".*(occStat).*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)

orphaned <-  strsplit(files_to_read2,'/')
orphaned = lapply (orphaned, function (x) x[9])
orphaned = unlist (orphaned)
orphaned<- data.frame(species_name=orphaned,
                      alpha_hull="orphaned_hole")
write_csv (x = orphaned,
           file = paste0(common_path2,'/orphaned_hole.csv'))
write_rds (orphaned,paste0(common_path2,'/orphaned_hole.csv'))

# alpha-hull====
common_path3<- "D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange"
# set up wd
setwd(common_path3)
### Read occurrence files of 29059 species from sub-directories###
files_to_read3 = list.files(
  path = common_path3,        # directory to search within
  pattern = ".*shp$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
alpha <-  strsplit(files_to_read3,'/')
alpha = lapply (alpha, function (x) x[8])
alpha = unlist (alpha)
alpha<- data.frame(species_name=alpha,
                      alpha_hull="Yes")
write_csv (x = alpha,
           file = paste0(common_path3,'/alpha_hull.csv'))
write_rds (orphaned,paste0(common_path3,'/alpha_hull.csv'))

# unique occurrence records less than 11 ====
occless10 <- setdiff(spClean$species_name,alpha$species_name)
occless10 <- setdiff(occless10,orphaned$species_name)
occless10 <- data.frame(species_name=occless10,
                        unique_occ_10="No")
occ10 <- data.frame(species_name=c(alpha$species_name,orphaned$species_name),
                    unique_occ_10="Yes")
# 3.occurrence records issue list====
occ <- rbind(occless10,occ10)

# 4.alpha hull list====
alpha_hull <- rbind(alpha,orphaned)

#  sdm list
common_path4.1<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/1"
# set up wd
setwd(common_path4.1)
### Read occurrence files of 838 species from sub-directories###
files_to_read4.1 = list.files(
  path = common_path4.1,        # directory to search within
  pattern = ".*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
sdm1 <-  strsplit(files_to_read4.1,'/')
sdm1 = lapply (sdm1, function (x) x[9])
sdm1 = unlist (sdm1)

common_path4.2<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/2"
# set up wd
setwd(common_path4.2)
### Read occurrence files of  species from sub-directories###
files_to_read4.2 = list.files(
  path = common_path4.2,        # directory to search within
  pattern = ".*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
sdm2 <-  strsplit(files_to_read4.2,'/')
sdm2 = lapply (sdm2, function (x) x[9])
sdm2 = unlist (sdm2)

common_path4.3<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/3"
# set up wd
setwd(common_path4.3)
### Read occurrence files of  species from sub-directories###
files_to_read4.3 = list.files(
  path = common_path4.3,        # directory to search within
  pattern = ".*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
sdm3 <-  strsplit(files_to_read4.3,'/')
sdm3 = lapply (sdm3, function (x) x[9])
sdm3 = unlist (sdm3)

common_path4.4<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4"
# set up wd
setwd(common_path4.4)
### Read occurrence files of  species from sub-directories###
files_to_read4.4 = list.files(
  path = common_path4.4,        # directory to search within
  pattern = ".*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
sdm4 <-  strsplit(files_to_read4.4,'/')
sdm4 = lapply (sdm4, function (x) x[9])
sdm4 = unlist (sdm4)

# species have built sdm
sdm <- c(sdm1,sdm2,sdm3,sdm4)
sdm <- data.frame(species_name=sdm,
                  SDM="Yes")
sdm_fail <- data.frame(species_name=setdiff(alpha$species_name,sdm$species_name),
                       SDM="No")
# 5.SDM list====
SDM <- rbind(sdm,sdm_fail)

# AIC issue====
common_path5.1<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/1"
#import statistics of species of which potential geographical range(rb) have been created
aicissue1 = list.files(pattern = '.stats.rds$',
                   path = common_path5.1,
                   recursive = T)
#select species that potential geographical range have been created and auc value is below 0.8
setwd(common_path5.1)
aicissue1= lapply(aicissue1, readRDS)
aicissue1 <- bind_rows(aicissue1)
aicissue1 <- aicissue1[aicissue1$AUC <0.8,"spName"] 

common_path5.2<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/2"
#import statistics of species of which potential geographical range(rb) have been created
aicissue2 = list.files(pattern = '.stats.rds$',
                       path = common_path5.2,
                       recursive = T)
#select species that potential geographical range have been created and auc value is below 0.8
setwd(common_path5.2)
aicissue2= lapply(aicissue2, readRDS)
aicissue2 <- bind_rows(aicissue2)
aicissue2 <- aicissue2[aicissue2$AUC <0.8,"spName"]

common_path5.3<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/3"
#import statistics of species of which potential geographical range(rb) have been created
aicissue3 = list.files(pattern = '.stats.rds$',
                       path = common_path5.3,
                       recursive = T)
#select species that potential geographical range have been created and auc value is below 0.8
setwd(common_path5.3)
aicissue3= lapply(aicissue3, readRDS)
aicissue3 <- bind_rows(aicissue3)
aicissue3 <- aicissue3[aicissue3$AUC <0.8,"spName"]

common_path5.4<- "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4"
#import statistics of species of which potential geographical range(rb) have been created
aicissue4 = list.files(pattern = '.stats.rds$',
                       path = common_path5.4,
                       recursive = T)
#select species that potential geographical range have been created and auc value is below 0.8
setwd(common_path5.4)
aicissue4= lapply(aicissue4, readRDS)
aicissue4 <- bind_rows(aicissue4)
aicissue4 <- aicissue4[aicissue4$AUC <0.8,"spName"]

# species with aic issues
aicissue <- c(aicissue1,aicissue2,aicissue3,aicissue4)
aicissue <- data.frame(species_name=aicissue,
                       AIC_issue="Yes")
aicok <- data.frame(species_name=setdiff(sdm$species_name,aicissue$species_name),
                    AIC_issue="No")
# 6.AIC list====
AIC <- rbind(aicok,aicissue)

# RU issue====
common_path6<- "D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar"
# set up wd
setwd(common_path6)
### Read occurrence files of  species from sub-directories###
files_to_read6 = list.files(
  path = common_path6,        # directory to search within
  pattern = 'staCen.rds$', # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
ru <-  strsplit(files_to_read6,'/')
ru = lapply (ru, function (x) x[8])
ru = unlist (ru)
ru <- data.frame(species_name=ru,
                 Range_unfilling="Yes")
ru_fail <- data.frame(species_name=setdiff(aicok$species_name,ru$species_name),
                      Range_unfilling="No")

# 7.RU list====
RU <- rbind(ru,ru_fail)

# realm issue====
# get names of species of which have issues with realms
sp_pro <- list.files(pattern = '.rds$',
                     path="D:/quercus/Partage/Pengcheng/R/exclude_pro_realm/species_2realms",
                     recursive = T)
spName = strsplit(sp_pro,'/')
spName = lapply (spName, function (x) x[1])
spName = unlist (spName)

# get names of species of which have issues with realms
sp_pro2 <- list.files(pattern = '.rds$',
                      path="D:/quercus/Partage/Pengcheng/R/exclude_pro_realm/species_3realms",
                      recursive = T)
spName2 = strsplit(sp_pro2,'/')
spName2 = lapply (spName2, function (x) x[1])
spName2 = unlist (spName2)

# get names of species of which have issues with realms
sp_pro3 <- list.files(pattern = '.rds$',
                      path="D:/quercus/Partage/Pengcheng/R/exclude_pro_realm/species_3realms",
                      recursive = T)
spName3 = strsplit(sp_pro3,'/')
spName3 = lapply (spName3, function (x) x[1])
spName3 = unlist (spName3)

#get names of species which are problematic in realm
spName_realm <- c(spName,spName2,spName3)
realm_ok <- setdiff(ru$species_name,spName_realm)
realm_fail <- setdiff(ru$species_name,realm_ok)
realm_ok <- data.frame(species_name=realm_ok,
                       Realm_ok="Yes")
realm_fail <- data.frame(species_name=realm_fail,
                       Realm_ok="No")
# 8.realm list====
realm <- rbind(realm_ok,realm_fail)

# CBI issue
sptodo = foreach(i=realm_ok$species_name) %do% {
  lapply (i, function (s) {grep (pattern = s,x = files_to_read6)})}
#sptodo =  unlist(sptodo) 
#in case unlist does not work
sptod<-c()
a<-1
foreach(i = sptodo) %do% {
  id <- unlist(i) 
  print(id)
  sptod[a]<-id
  a<-a+1
}
#  select CBI below 0.8
CBI = lapply(files_to_read6[sptod], readRDS)
CBI <-dplyr::bind_rows(CBI)
CBIfail <- unique(CBI[CBI$CBI1<0.8,"spName"])
CBIfail <- na.omit(CBIfail)
CBIok <- unique(CBI[CBI$CBI1>=0.8,"spName"])
CBIok <- na.omit(CBIok)
CBIfail <- data.frame(species_name=CBIfail,
                      CBI_ok="No")
CBIok <- data.frame(species_name=CBIok,
                    CBI_ok="Yes")
# 9.CBI list====
cbi <- rbind(CBIok,CBIfail)

# 10.species in the analysis====
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code_dataset_RU")
# mean values of explanatory variables
mean_df_ana2 <- readRDS("mean_df_ana2.rds")
final_species <- mean_df_ana2[,"spName"]
nonfinal <- setdiff(CBIok$species_name,final_species)
final_species <- data.frame(species_name=final_species,
                            Final_species="Yes")
nonfinal <- data.frame(species_name=nonfinal,
                       Final_species="No")
species <- rbind(final_species,nonfinal)

# 11.final dataset====
Species_list<- list(sporignal,spclean,occ,alpha_hull,SDM,AIC,RU,realm,cbi,species)
a <- Species_list %>% reduce(full_join, by='species_name')

# output
write_csv (a,
           file = "D:/quercus/Partage/Pengcheng/R/analyse/annex/species_list.csv")
write_rds (a,file = "D:/quercus/Partage/Pengcheng/R/analyse/annex/species_list.rds")

