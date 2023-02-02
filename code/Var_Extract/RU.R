rm(list=ls())  ###start from scratch
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4")
##set up library
packs <- list("geometry",'doParallel','foreach',"tidyverse",'mapview','raster','terra',"gtools",
              "data.table","sf",'Hmisc',"pastecs")
lapply(packs, require, character.only = T)


# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =8
cl <- makeCluster(mc.cores)
registerDoParallel(cl)


# set path for temporary files
terraOptions(memfrac=0.9, tempdir = "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/tem")
rasterOptions(tmpdir="D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/tem")

###import data====
#1
#import statistics of species of which potential geographical range(rb) have been created
spPRa = list.files(pattern = '.stats.rds$',
                   recursive = T)
spPRan = strsplit(spPRa,'/')
spPRan = lapply (spPRan, function (x) x[1])
spPRan = unlist (spPRan)

#2
#get the path of binary result of potential geographical range of species
PGR = list.files(pattern = '.bina.tif$',
                 path = 'D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4',
                 recursive = T)

# get species id
# (sp <-grep("Afzelia_parviflora",x=PGR))


#select species that potential geographical range have been created and auc value is above 0.8
sta_ls = lapply(spPRa, readRDS)
sta_ls <- bind_rows(sta_ls)
spR_aucok <- sta_ls[sta_ls$AUC >=0.8,"spName"] 

#3
#get species id which species RU and explainable variables have been created
spDone = list.files(pattern = 'sta.rds$',
                    path = 'D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar_2',
                    recursive = T)
spDone = strsplit(spDone,'/')
spDone = lapply (spDone, function (x) x[1])
spDone = unlist (spDone)

#get species id which species Range filling is 100%
spDone2 = list.files(pattern = '.tif$',
                     path = 'D:/quercus/Partage/Pengcheng/R/variable_extraction/RF_100%_2',
                     recursive = T)
spDone2 = strsplit(spDone2,'/')
spDone2 = lapply (spDone2, function (x) x[1])
spDone2 = unlist (spDone2)

spDone <- c(spDone,spDone2)

#4
# remove species already done
spR_aucok <- spR_aucok [!(spR_aucok  %in% spDone )]
#get ids of species with PGR and ok auc from species list that originated from alpha hull
sptodo <- spPRan [(spPRan  %in% spR_aucok)]
sptodo = foreach(i=sptodo) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = spPRa)})}
#sptodo =  unlist(sptodo) #does not work
sptod<-c()
a<-1
foreach(i = sptodo) %do% {
  id <- unlist(i) 
  print(id)
  sptod[a]<-id
  a<-a+1
}

#load climate variables
bioVars = list.files (path = 'D:/quercus/BD_SIG/climat/monde/CHELSA_v2/bio',pattern = '_bio',full.names = T)
clim = terra::rast(mixedsort(bioVars)) #template for rasterlization of geographical range

#build a template for equal area projection
ref_grid<-extent(-180, 180, -90, 90)
ref_grid<-raster(ref_grid)
res(ref_grid)<-1
values(ref_grid)<-1 #dummy values
projection(ref_grid)<-CRS("+proj=longlat +datum=WGS84 +no_defs    +ellps=WGS84 +towgs84=0,0,0") 
ref_gridProj<-projectRaster(ref_grid, crs="+proj=eck4 +datum=WGS84", res=9800)
ref_gridProj <- terra::rast(ref_gridProj)
#init(ref_gridProj, prod(res(ref_gridProj)))

#5
#read sf object of ecoregion(realms)
ecoregion <- st_read("D:/quercus/Partage/Pengcheng/R/cleaing_data/Ecoregion_realms/Ecoregions2017/Ecoregions2017.shp")

#6
#path of species which geographical range(a-hull,gr) have been created
spRan = list.files(pattern = '.shp$',
                   path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange',
                   recursive = T)


#7
#read sf object dgg_tree_hm which contains tree cover, 
#adjusted tree cover, human modification
### and climatic variables in different years and periods (in median values)
dgg_tree_hm <- readRDS("D:/quercus/Partage/Pengcheng/R/HM_TC/data/dgg_tree_hm.rds")
#View(dgg_tree_hm)
dgg_tree_hm= dgg_tree_hm[,c(1,7,13,14,93,95,97,99,101,103,107,108,116:136,146:196)]
dgg_tree_hm = st_transform(dgg_tree_hm,
                           crs="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
dgg_tree_hm1 <- terra::vect(dgg_tree_hm)
datanames<- names(dgg_tree_hm1)
dataequal=foreach(i = datanames,.combine=c) %do% {
  #dgg_tree_hm2[[i]] <- terra::rasterize(dgg_tree_hm1,ref_gridProj,field=i)
  dgg_tree_hm2 <- terra::rasterize(dgg_tree_hm1,ref_gridProj,field=i)
}  ##raterize the database with Eckert IV equal-area Projection 
# plot(dataequal$tr_median)

# save the result
# outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/HM_TC/data')
# dir.create(outputSpDir)
# writeRaster(x = dataequal,
#             file = paste0(outputSpDir,'/equalarea.tif'),overwrite=TRUE)
##or rasterize by loop
# foreach(i = a) %do% {
#   dgg_tree_hm2[[i]] <- terra::rasterize(dgg_tree_hm1,ref_gridProj,field=i)
# 
# }
# reload the result
# dataequal <- terra::rast("D:/quercus/Partage/Pengcheng/R/HM_TC/data/equalarea.tif")

# Stop the cluster
stopCluster(cl)
closeAllConnections()
registerDoSEQ()    ###do the sequential coding

#the loop ==== 
l=foreach(spi = sptod,
          .packages = as.character(packs),
          .errorhandling = 'pass') %do% {

            #set up working directory 
            setwd("D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4")
            ##need to change work directory
            
            # get potential geographical range(pgr) which is bigger than geographical range since it not only
            # includes the ecoregions where points fall in, also incorporate the adjacent ecoregions
            pgr <-terra::rast(PGR[spi]) 
            pgr<- terra::resample(pgr,ref_gridProj,method="near") #change crs and resolution
            #plot(pgr) 
            spName <- basename(PGR[spi])
            spName <- strsplit(spName,"_bina.tif")[[1]]  #get species name in order to match species in a-hull
            
            
            #make sure the species name is in exact match
            gr = grep(paste0(spName,".shp$") ,x=spRan)  #get species id in geographical range(a-hull) session
            model_val = grep(paste0(spName,"stats.rds$"),x=spPRa) #get species id in potential geographical range(pgr) session
            
            model_val <- readRDS(spPRa[model_val]) #get model validation info
            
            #set up working directory 
            setwd("D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange")
            gr <- st_read(spRan[gr])
            #set crs
            st_crs(gr)=st_crs(ecoregion)
            
            #to void overlapping
            sf_use_s2(FALSE)
            # realm <- st_intersection(gr,ecoregion)
            # #select most frequent realm as final realm
            # # should directly based on realm rather than use ecoregion as media
            # realm<- na.omit(realm)
            # realm <- names(sort(table(realm$REALM),decreasing=TRUE)[1])  
            
            #mapview(gr)
            gr <- terra::vect(gr)
            gr <- terra::rasterize(gr,clim[[1]]) 
            #same resolution as pgr to calculate RU
            gr<- terra::resample(gr,ref_gridProj,method="near") #change crs and resolution
            gr <- terra::trim(gr) # get rid of extra NA space
            #plot(gr)
            em = terra::merge(pgr,gr) #get biggest extent from potential geographical range and geographical range
            gr <- terra::extend(gr,em) #extend spatial extent as em
            pgr <- terra::extend(pgr,em) # just in case
            
            #transform NA into 0 
            # in order to get statistic of space where exists the presence of species by pgr
            gr[is.na(gr)] <- 0 
            
            # made a new map named sta_map based on potential geographical range and geographical range
            # meaning of the values of new map: 0 represents this pixel is predicted as unsuitable from either
            # potential geographical range(pgr) and geographical range(gr) perspective; 1 represents this pixel
            # is only considered as suitable from gr; 10 represents this pixel is only thought as
            # suitable from pgr by excluding areas fall into 11; 11 represents this pixel is believed as suitable from 
            # both pgr and gr views. the ideal result is that we get 1 as less as possible
            # Note 1,10 might not exsit
            
            sta_map <- 10*pgr + gr 
            #plot(sta_map)
            sta <- terra::as.data.frame(sta_map,na.rm=T)
            names(sta)<- "probalility"
            sta <- sta %>% # get statistics of sta_map
              count(probalility)
            
            
            # the higher the value of RU is, the more limitations existing for species to expand 
            # ratio between suitable habitat which is not occupied by the species and habitat the species
            # exist(inside/ inside and outside suitable habitat)
            if ( 10 %in% sta$probalility) {
              
              model_val$RU_onlyIn<- sta$n[sta$probalility==10]/ sta$n[sta$probalility==11]
              if ( 1 %in% sta$probalility) {
                model_val$RU_inout <- sta$n[sta$probalility==10]/(sta$n[sta$probalility==11] + sta$n[sta$probalility==1])
              } else { model_val$RU_inout <- NA
              
              }
              
              #only keep the habitat where environment is predicted to be suitable for species to exist
              pgr[pgr==0] <- NA  #left value 1
              #new spatraster with explainable variables for range unfilling of this species
              spData <- dataequal * pgr  
              # plot(spData$tr_median)
              # spDat <- terra::global(spData,fun="mean",na.rm=T) do not have a function with median value option
              # get the dataframe of Spatraster since neither global nor cellstastic have function of median
              spData <- terra::as.data.frame(spData,na.rm=T)
              spData $id<-NULL 
              spData<- na.omit(spData)
              spSta <- stat.desc(spData) # get statistics
              spSta <- spSta[c("median","mean"),] # select median and mean value
              spSta$sta <- c("median","mean")
              model_val[rep(1, 2),]
              spSta <- cbind(model_val,spSta
                             # ,realm add realm in adding centriod step
                             ) # comine the results
              
              gc()
              #output
              outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar_2/',spName)
              dir.create(outputSpDir, recursive = T)
              writeRaster(x = sta_map,
                          file = paste0(outputSpDir,'/',spName,'_sta.tif'),overwrite=TRUE)
              write_csv (x = spSta,
                         file = paste0(outputSpDir,'/',spName,'_sta.csv'))
              write_rds (spSta,paste0(outputSpDir,'/',spName,'_sta.rds'))
              
              
              
              ##remove important something out of list to save RAM 
              rm(list = c('sta','sta_map','em',"gr" ,"pgr"))
              #clean RAM
              gc()
              tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE) #remove temporary file
            } else {
              #output
              outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/variable_extraction/RF_100%_2/',spName)
              dir.create(outputSpDir, recursive = T)
              writeRaster(x = sta_map,
                          file = paste0(outputSpDir,'/',spName,'_sta.tif'),overwrite=TRUE)
              write_csv (x = sta,
                         file = paste0(outputSpDir,'/',spName,'_sta_map.csv'))
              write_rds (sta,paste0(outputSpDir,'/',spName,'_sta_map.rds'))
              
              ##remove important something out of list to save RAM 
              rm(list = c('sta','sta_map','em',"gr" ,"pgr"))
              #clean RAM
              gc()
              tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE) #remove temporary file
            }
            
          }

gc()
View(l)  ###check the results if there is any errors
