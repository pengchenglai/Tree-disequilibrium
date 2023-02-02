## Set up files and libraries ====
rm(list=ls())  ###start from scratch
setwd("D:/quercus/Partage/Pengcheng/R/cleaing_data")
#set up library
packs <- list("tidyverse", "countrycode", "sf", "terra","stars","rgdal","data.table","rgbif",
              "sp","foreach","stringr","CoordinateCleaner","mapview",'rgdal',
              "pbapply",'doParallel','foreach',"rangeBuilder","readxl")
lapply(packs, require, character.only = T)


# Main directory where all the sub-dir with files are stored ====
common_path = "D:/quercus/Partage/Pengcheng/20211220"
### Read occurrence files of 52168 species from sub-directories###
files_to_read = list.files(
  path = common_path,        # directory to search within
  pattern = ".*^(occ).*csv$", # regex pattern,  careful with "occ" in the string
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
#sum(str_detect(files_to_read,"/occ"))

# should not read all the matching files  ###is too big
###data_lst = lapply(files_to_read, read.csv)  




#in case need to select a nominated species
##a <- as.data.table(files_to_read)
###(b <-grep("Alluaudiopsis_fiherenensis",x=a$files_to_read))
###(b <-a[grep("Cercidiphyllum",x=a$files_to_read),])




# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =20
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

#To redo the problematic record ====
###To see  complete list of unique names of species
allSp = strsplit(files_to_read,'/')
allSp = lapply (allSp, function (x) x[6])
allSp = unlist (allSp)

###For checking errors debugging process
#check species names what have been done in foreach
#spDone = list.files(pattern = '.rds$',
#           path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc',
#           recursive = T)
#spDone = strsplit(spDone,'/')
#spDone = lapply (spDone, function (x) x[1])
#spDone = unlist (spDone)

#check species names what have been done in foreach
spDone = list.files(pattern = '.shp$',
                    path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange',
                    recursive = T)
spDone = strsplit(spDone,'/')
spDone = lapply (spDone, function (x) x[1])
spDone = unlist (spDone)

###For checking errors debugging process
#check species names which do not have occurrence record passing the native country examination
#spNoO = list.files(pattern = '.csv$',
#                    path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/No_occ(No_country)',
#                    recursive = T)
#spNoO = strsplit(spNoO,'/')
#spNoO = lapply (spNoO, function (x) x[1])
#spNoO = unlist (spNoO)

#speces to do(list the species have not done the potential range and redo the whole process)
spToDo = setdiff(allSp,spDone)
#spToDo <- setdiff(spToDo,spNoO) ###debugging process
id_toD = foreach(i=spToDo) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = files_to_read)})}
# or
#id_toDo = pblapply (spToDo, function (s) {grep (pattern = s,x = files_to_read)})
#id_toD <- sapply (id_toD, function (x) x[1])
id_toDo<-c()
a<-1
foreach(i = id_toD) %do% {
  id <- unlist(i) 
  print(id)
  id_toDo[a]<-id
  a<-a+1
}
##something wrong with unlist
#id_toDo =  unlist(id_toD)
##id_toDo[duplicated(id_toDo) %in% id_toDo]
#id_toDo =  unique(unlist(id_toDo))
######error 4: missing species name(cannot fixed)====
id_toDo=id_toDo[!id_toDo %in% c(26334,29948)]  ###these id are without species name(the Latin name)



#Build a loop =====   
l=foreach(spi = row_number(files_to_read),
          .packages = as.character(packs),
          .errorhandling = 'pass') %dopar% {
df<-read_csv (files_to_read[spi])
#View(df)

# species name 
###?strsplit
spName = basename (files_to_read[spi])
spName = strsplit (spName,'.csv')[[1]]
spName = strsplit (spName,'occ_')[[1]][2]

####error 3: zero records in the original df(cannot fixed)====
if(nrow(df)==0){
  outputSpDir1 = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/No_occ(No_country)/',spName)
dir.create(outputSpDir1, recursive = T)
write_csv (x = df,
           file = paste0(outputSpDir1,'/',spName,'-occ.csv'))
}else{

#select columns of interest
checkColNames = c('species', 'decimalLongitude', 'decimalLatitude', 'countryCode', 'individualCount',
'gbifID', 'family', 'taxonRank', 'coordinateUncertaintyInMeters', 'year',
'basisOfRecord', 'institutionCode', 'datasetName')

dat =   df [, checkColNames[checkColNames %in% names (df)]]
#or other way
# dat <-  df %>%
#   dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
#                 gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
#                 basisOfRecord, institutionCode, datasetName)


# remove records without coordinates  
##error 5:missing coordinates
##: for example,10368 
###name the %!in% 
`%!in%` <- Negate(`%in%`)
if('decimalLongitude' %!in% names(dat) | 'decimalLatitude' %!in% names(dat))
  {outputSpDir4 = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/No_occ(No_country)/',spName)
  dir.create(outputSpDir4, recursive = T)
  write_csv (x = df,
             file = paste0(outputSpDir4,'/',spName,'-occ.csv'))}
else{
dat <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

#plot data to get an overview
# wm <- borders("world", colour="gray50", fill="gray50")
# ggplot()+ coord_fixed()+ wm +
#   geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
#              colour = "darkred", size = 0.5)+
#   theme_bw()
 
#convert country code from ISO2c to ISO3c
####error 0:value of country code is NA((cannot fixed))==== 
if(nrow(dat[!is.na(dat$countryCode),])>0) {  
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')
} else {outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/No_country/',spName)
dir.create(outputSpDir, recursive = T)
write_csv (x = dat,
           file = paste0(outputSpDir,'/',spName,'-occ.csv'))
} ####although without countrycode may also pass  native country test if the species is not in the list

#View(dat)
#Native record====
#import df which contain native species records in each country
native<- read.csv("D:/quercus/Partage/Pengcheng/Summary/Data/Native species_Julia/Species_in_each_Country.csv",                     
                     header=TRUE)
### check data if it is right 
####head(native)
#### error 1 of country code,examined by debugging process(fixed)====
###code of New Zheland is wrong, changing into "NZL" 
native[native$file=="NLZ",]$file <- "NZL" ###See details below
native[native$file=="CSK",]$file <- "CZE" ###See details below

####Import iso3c country code
#countrycodeonline <- read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")
#nrow(countrycodeonline)
####import country code in our database
#countrycodenative <- read_excel("D:/quercus/Partage/Pengcheng/Summary/Data/Native species_Julia/0_Ctry_list.xlsx")
#nrow(countrycodenative)
#countrycodeonline$Alpha.2.code1 <- stringr::str_replace_all(countrycodeonline$Alpha.2.code," ","")
##move space in Alpha.2.code
#countrytest <- merge(countrycodeonline,countrycodenative,by.x="Alpha.2.code1",by.y= "Alpha2")
#nrow(countrytest)
##move space in Alpha.3.code
#countrytest$Alpha.3.code1 <- stringr::str_replace_all(countrytest$Alpha.3.code," ","")
#countrytest[!countrytest$Alpha.3.code1 == countrytest$Alpha3,]


####add an underline to match spName 
native$taxon1 <- stringr::str_replace_all(native$taxon," ","_")
###if species name is in the native dataframe then do the following code to select native records
if( spName %in% native$taxon1 ){
a <- native[native$taxon1 %in% spName,]$file
###Select species records in native country###
dat$native_range<-ifelse(dat$countryCode%in% a , 1, 0)
dat <- dat[dat$native_range==1,]
}

####native[native$taxon1==spName,"file"]

###Flag problems ====
if(nrow(dat)>0){
flags <- CoordinateCleaner::clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries")) # most test are on by default
# summary(flags)
# plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

#Exclude problematic records
dat_ok <- dat[flags$.summary,]
okDat= nrow(dat_ok)
#The flagged records
dat_fail <- dat[!flags$.summary,]
flaggedDat= nrow(dat_fail)

#Remove records with low coordinate precision(>50 km) optinal
# hist(dat_ok$coordinateUncertaintyInMeters / 1000, breaks = 20)
####A histogram of the coordinate precision in the dataset#
# dat_ok <- dat_ok %>%
#   filter(coordinateUncertaintyInMeters / 1000 <= 50 | is.na(coordinateUncertaintyInMeters))
# View(dat_ok)

#Remove unsuitable data sources, especially fossils 
#which are responsible for the majority of problems in this case
dat_ok <- filter(dat_ok, basisOfRecord == "HUMAN_OBSERVATION" |
                   basisOfRecord == "OBSERVATION" |
                   basisOfRecord == "PRESERVED_SPECIMEN")
UnsuitFilter<- nrow(dat_ok)


#Individual count
###Move unsuitable records
####error 2 column may not exists(fixed)====
if( "individualCount" %in% names(dat_ok)){
  
  dat_ok <- dat_ok%>%
    filter(individualCount > 0 | is.na(individualCount))%>%
    filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem
  indivCountFilter = nrow(dat_ok)
}

#Age of records
###error 2 column may not exists(fixed)====
if('year' %in% names(dat_ok)){
  dat_ok <- dat_ok%>%
    filter(year > 1945) # remove records from before second world war
  yearInfo = nrow(dat_ok)}

###?cd_ddmm  May not be neccessary
####Identify dataset with ddmm to dd.dd conversion error
#dat_ok <- cd_ddmm(dat_ok, lon = "decimalLongitude", lat = "decimalLatitude",
#                    ds = "datasetName", diagnostic = F, diff = 1,
#                    value = "clean")

###Test for rasterized sampling
# out.round <- cd_round(dat_ok, lon = "decimalLongitude",
#                       lat = "decimalLatitude",
#                       ds = "datasetName",
#                       value = "dataset",
#                       T1 = 7,
#                       graphs = F)


# range creation requires at least 10 unique occurrences.###
count_occ_unique<-nrow (unique (dat_ok[,c('decimalLatitude','decimalLongitude')]))


#write output until now 
###if we have column called "individualCount" and "year" in dat_ok then do the following code to register how many records
##left or filtered in each step; otherwise if "individualCount" is absent and  while we have "year" , then do 
##first else command;otherwise if "individualCount" is present and  while we do not have "year",then do second
## else commond;if neither of these two column is inside of dat_ok, then do the last else commond

if( "individualCount"  %in% names(dat_ok) & "year"  %in% names(dat_ok)){
  countsOcc = data.frame (spName ,
                          Nini = nrow (dat), 
                          #### species record from original data frame which may go through native country test 
                          ###depend on whether it is contained in the df of native species records in each country
                          Nend = nrow (dat_ok), 
                          ####number of records kept after all the selection procedures except duplication
                          Nendunique = count_occ_unique, #####exclude duplicate records###
                          filterflagged = flaggedDat, ### records failed to pass the default examinations of 
                          ##CoordinateCleaner
                          filterunsuitable= okDat- UnsuitFilter, ####records fail the suitable test
                          filterIndivCount = UnsuitFilter-indivCountFilter,####records fail the indivCount test
                          filterYear = indivCountFilter - yearInfo) ####records fail the year test
  ##filterdegree=yearInfo-nrow(dat_ok) records fail the degree test,removing majority of 
  ##records
} else if ("individualCount" %!in% names(dat_ok) & "year"  %in% names(dat_ok)){
  countsOcc = data.frame (spName ,  
                          Nini = nrow (dat), 
                          #### species record from original data frame which may go through native country test 
                          ###depend on whether it is contained in the df of native species records in each country
                          Nend = nrow (dat_ok), 
                          ####number of records kept after all the selection procedures except duplication
                          Nendunique = count_occ_unique, #####exclude duplicate records###
                          filterflagged = flaggedDat, ### records failed to pass the default examinations of 
                          ##CoordinateCleaner
                          filterunsuitable= okDat- UnsuitFilter, ####records fail the suitable test
                          filterYear=UnsuitFilter-yearInfo)####records fail the year test
  ##filterdegree=yearInfo-nrow(dat_ok) records fail the degree test,removing majority of 
  ##records
} else if ("individualCount" %in% names(dat_ok) & "year"  %!in% names(dat_ok)){
  countsOcc = data.frame (spName ,  
                          Nini = nrow (dat), 
                          #### species record from original data frame which may go through native country test 
                          ###depend on whether it is contained in the df of native species records in each country
                          Nend = nrow (dat_ok), 
                          ####number of records kept after all the selection procedures except duplication
                          Nendunique = count_occ_unique, #####exclude duplicate records###
                          filterflagged = flaggedDat, ### records failed to pass the default examinations of 
                          ##CoordinateCleaner
                          filterunsuitable= okDat- UnsuitFilter, ####records fail the suitable test
                          filterIndivCount = UnsuitFilter-indivCountFilter)####records fail the indivCount test
} else {countsOcc = data.frame (spName ,  
                                Nini = nrow (dat), 
                                #### species record from original data frame which may go through native country test 
                                ###depend on whether it is contained in the df of native species records in each country
                                Nend = nrow (dat_ok), 
                                ####number of records kept after all the selection procedures except duplication
                                Nendunique = count_occ_unique, #####exclude duplicate records###
                                filterflagged = flaggedDat, ### records failed to pass the default examinations of 
                                ##CoordinateCleaner
                                filterunsuitable= okDat- UnsuitFilter)####records fail the suitable test
}





###output the results as csv,rds 
outputSpDir2 = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc/',spName)
dir.create(outputSpDir2, recursive = T)
write_csv (x = countsOcc,
           file = paste0(outputSpDir2,'/',spName,'-occStat.csv'))

write_rds (dat_ok,
           file = paste0(outputSpDir2,'/',spName,'-occNonUnique.rds'))
write_csv (dat_ok,
           file = paste0(outputSpDir2,'/',spName,'-occNonUnique.csv'))


#Do geographic range ====
###dat_ok_coordinates_sf<-st_as_sf(dat_ok_coordinates,coords=c('decimalLongitude', 'decimalLatitude'),crs=st_crs(4326))
##plot(dat_ok_coordinates_sf[,1])
##mapview(dat_ok_coordinates_sf)


###error 5 orphaned hole(cannot fixed) ====
### skip first if and set condition count_occ_unique >=10 in second if
# if(count_occ_unique >=11){
# outputSpDir4 = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/largeOcc/',spName)
# dir.create(outputSpDir4, recursive = T)
# write_csv (x = countsOcc,
#            file = paste0(outputSpDir4,'/',spName,'-occStat.csv'))
# 
# write_rds (dat_ok,
#            file = paste0(outputSpDir4,'/',spName,'-occNonUnique.rds'))
# write_csv (dat_ok,
#            file = paste0(outputSpDir4,'/',spName,'-occNonUnique.csv'))
# 
#   
# }else 
  if (count_occ_unique >10 ){
  
  #MAKE UNIQUE COORDINATES
  dat_ok_coordinates  = unique (dat_ok[,c('decimalLatitude','decimalLongitude')])
  dat_ok_coordinates<-dat_ok_coordinates[,c(2,1)]
  
  #use range Builder to do the species ranges.
  #library(rangeBuilder)
  
  georange <- rangeBuilder::getDynamicAlphaHull(dat_ok_coordinates, fraction = 0.95, partCount = 5, buff = 0,
                                                initialAlpha = 3,
                                                clipToCoast = 'terrestrial', proj = "+proj=longlat +datum=WGS84",
  )
  
  ### ?getDynamicAlphaHull
  #select polyon (sf object) of species range
  sf_range<-st_as_sf(georange[[1]])
  #plot(st_geometry(sf_range))
  #mapview(sf_range)
  ###creat a file to store data as rds
  outputSpDir3 = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange/',spName)
  dir.create(outputSpDir3, recursive = T)
  saveRDS (georange,file =paste0(outputSpDir3,'/',spName,'_rangeBuilder.rds') )
  ###number of maximum alphahull is used in building this polygon
  alphaNumber = strsplit (georange[[2]],'alpha')[[1]][2]
  ####output the alphuahull information
  alphaHullSp_Info=data.frame (spName=spName,alpha=alphaNumber)
  saveRDS (alphaHullSp_Info,file =paste0(outputSpDir3,'/',spName,'_alphaInfo.rds') )
  ###save a shp file
  st_write(sf_range, dsn = outputSpDir3, layer = paste0(spName,".shp"), driver = "ESRI Shapefile",append=F)
  print(spName)
} else { print (spName)}

} else {  #### error 3 as it may exists zero records(cannot fixed)====
  ####include not passing native country test and coordinate test with row 206 in this script
outputSpDir1 = paste0('D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc_problem/No_occ(No_country)/',spName)
dir.create(outputSpDir1, recursive = T)
write_csv (x = df,
           file = paste0(outputSpDir1,'/',spName,'-occ.csv'))
}
}
}
}

# Stop the cluster
stopCluster(cl)
closeAllConnections()
registerDoSEQ()    ###do the sequential coding
View(l)  ###check the results if there is any errors


