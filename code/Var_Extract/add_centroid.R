rm(list=ls())  ###start from scratch
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar_2")
##set up library
packs <- list("tidyverse",'doParallel','foreach',"sf",'mapview')
lapply(packs, require, character.only = T)

# get path of individual species statistics and species name
df_sp <- list.files(pattern = 'sta.rds$',
                    recursive = T)
spNam = strsplit(df_sp,'/')
spNam = lapply (spNam, function (x) x[1])
spNam = unlist (spNam)

# import species which occurrence data have been cleaned
spOc = list.files(pattern = '.-occNonUnique.rds$',
                  path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc',
                  recursive = T)
spOcc = strsplit(spOc,'/')
spOcc = lapply (spOcc, function (x) x[1])
spOcc = unlist (spOcc)

#path of species which geographical range(a-hull,gr) have been created
spRan = list.files(pattern = '.shp$',
                   path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange',
                   recursive = T)

# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =15
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

#get species id from cleaned occurrence records based on species name
spname <- spOcc[spOcc %in% spNam]
sptodo = foreach(i=paste0(spname,"/")) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = spOc)})}
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

#read sf object of ecoregion(realms)
ecoregion <- st_read("D:/quercus/Partage/Pengcheng/R/cleaing_data/Ecoregion_realms/Ecoregions2017/Ecoregions2017.shp")
#to void overlapping
sf_use_s2(FALSE)
# merge ecoregions into 8 realms
ecoregion_union <- ecoregion %>% 
  mutate(REALM1 = as.character(REALM),
         REALM1 = ifelse(is.na(REALM), "NA", REALM)) %>% 
  group_by(REALM1) %>% 
  summarise(geometry = st_union(geometry),
            area= sum(SHAPE_AREA))
# plot(ecoregion_union)
ecoregion_union <- ecoregion_union[!ecoregion_union$REALM1=="N/A",]

#the loop ====
l=foreach(spi = sptod,
          .packages = as.character(packs),
          .errorhandling = 'pass') %dopar% {
            
            #set up working directory 
            setwd("D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc")
            occ <-readRDS (spOc [spi])
            occ = unique (occ[,c('decimalLongitude','decimalLatitude')])
            occ <- na.omit(occ) # get unique occurrence records
            spName = basename (spOc [spi]) # get species name
            spName = strsplit (spName,'-occNonUnique.rds')[[1]]
            Long <- mean(occ$decimalLongitude)
            Lat <- mean(occ$decimalLatitude)
            df <- data.frame(Long, Lat, stringsAsFactors = FALSE) # put centroid coordinates of the species in a dataframe
            
            #make sure the species name is in exact match
            gr = grep(paste0(spName,".shp$") ,x=spRan)  #get species id in geographical range(a-hull) session
            #set up working directory 
            setwd("D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange")
            gr <- st_read(spRan[gr])
            # mapview(gr)
            #set crs
            st_crs(gr)=st_crs(ecoregion)
            #to void overlapping
            sf_use_s2(FALSE)
            # good way to select realm with biggest intersection area
            realm<- st_join(gr, ecoregion_union, join = st_intersects,largest = TRUE)
            realm1 <- realm$REALM1
            # realm <- st_intersection(gr,ecoregion_union)
            # select most frequent realm as final realm
            # realm1 <- names(sort(table(realm$REALM1),decreasing=TRUE)[1])  
              
            #set up working directory 
            setwd("D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar_2")
            #make sure the species name is in exact match
            id = grep(paste0(spName,"_sta.rds$") ,x=df_sp)  #get species id in Range Unfilling session
            statistics <-readRDS (df_sp [id])
            spSta <- cbind(df,statistics,data.frame(realm1)) # new dataset with centroid coordinates and realm
            
            gc()
            #output
            outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar_2/',spName)
            dir.create(outputSpDir, recursive = T)
            write_csv (x = spSta,
                       file = paste0(outputSpDir,'/',spName,'_staCen.csv'))
            write_rds (spSta,paste0(outputSpDir,'/',spName,'_staCen.rds'))
          }           

# Stop the cluster
stopCluster(cl)
closeAllConnections()
registerDoSEQ()    ###do the sequential coding

