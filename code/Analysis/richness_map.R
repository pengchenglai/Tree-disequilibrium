#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code")
# mean values of explanatory variables
mean_df_ana2 <- readRDS("mean_df_ana2.rds")

##set up library
packs <- list("tidyverse",'doParallel','foreach','mapview',"sf","Hmisc","terra","raster")
lapply(packs, require, character.only = T)

# get tree species richness map ====
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar")
spName<- mean_df_ana2$spName

# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =10
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

sta_mp = list.files(pattern = '._sta.tif$',
                    recursive = T)
sptodo = foreach(i=paste0(spName,"_sta")) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = sta_mp)})}
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

#build a template for equal area projection
ref_grid<-extent(-180, 180, -90, 90)
ref_grid<-raster(ref_grid)
res(ref_grid)<-1
values(ref_grid)<-0 #dummy values
projection(ref_grid)<-CRS("+proj=longlat +datum=WGS84 +no_defs    +ellps=WGS84 +towgs84=0,0,0") 
ref_gridProj<-projectRaster(ref_grid, crs="+proj=eck4 +datum=WGS84", res=9800)
ref_gridProj <- terra::rast(ref_gridProj)
#init(ref_gridProj, prod(res(ref_gridProj)))
# unfilled <- deepcopy(ref_gridProj) # empty template
# filled_onlyin <- deepcopy(ref_gridProj) # empty template
filled_inout <- deepcopy(ref_gridProj) # empty template
sdm_tem <- deepcopy(ref_gridProj) # empty template


#the loop ==== 
l=foreach(spi = sptod,
          .packages = as.character("terra")) %do% {
            sta_map <- rast(sta_mp[spi])
            # plot(sta_map)
            # reminder: sta_map <- 10*pgr + gr 
            # made a new map named sta_map based on potential geographical range and geographical range
            # meaning of the values of new map: 0 represents this pixel is predicted as unsuitable from either
            # potential geographical range(pgr) and geographical range(gr) perspective; 1 represents this pixel
            # is only considered as suitable from gr; 10 represents this pixel is only thought as
            # suitable from pgr by excluding areas fall into 11; 11 represents this pixel is believed as suitable from 
            # both pgr and gr views. the ideal result is that we get 1 as less as possible
            # Note 1,10 might not exsit
            # sta_unfilled <- sta_map==10 
            # # plot(sta_unfilled)
            # sta_unfilled <- terra::resample(sta_unfilled,ref_gridProj,method="near") 
            # sta_unfilled[is.na(sta_unfilled)] <- 0
            # unfilled <- unfilled + sta_unfilled
            # # plot(unfilled)
            
            sdm <- sta_map==10 | sta_map==11
            # plot(sdm)
            sdm <- terra::resample(sdm,ref_gridProj,method="near") 
            sdm[is.na(sdm)] <- 0
            sdm_tem <- sdm_tem + sdm
            # plot(sdm_tem)
            
            # sta_filled_onlyin <- sta_map==11
            # # plot(sta_filled_onlyin)
            # sta_filled_onlyin <- terra::resample(sta_filled_onlyin,ref_gridProj,method="near")
            # sta_filled_onlyin[is.na(sta_filled_onlyin)] <- 0
            # filled_onlyin <- filled_onlyin + sta_filled_onlyin
            # # plot(filled_onlyin)
            # 
            # sta_filled_inout <- sta_map==11 | sta_map==1
            # # plot(sta_filled_inout)
            # sta_filled_inout <- terra::resample(sta_filled_inout,ref_gridProj,method="near")
            # sta_filled_inout[is.na(sta_filled_inout)] <- 0
            # filled_inout <- filled_inout + sta_filled_inout
            # # plot(filled_inout)
            gc()
            #output 
            outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/tree_richness/')
            dir.create(outputSpDir, recursive = T)
            # writeRaster(x = unfilled,
            #             file = paste0(outputSpDir,'/unfilling.tif'),overwrite=TRUE)
            # # writeRaster(x = filled_onlyin,
            #             # file = paste0(outputSpDir,'filling_onlyin.tif'),overwrite=TRUE)
            # # writeRaster(x = filled_inout,
            #             file = paste0(outputSpDir,'filling_inout.tif'),overwrite=TRUE)
            writeRaster(x = sdm_tem,
                        file = paste0(outputSpDir,'/sdm.tif'),overwrite=TRUE)
          }
# names(unfilled) <- "unfilling"
# names(filled_onlyin) <-  "filling_onlyin"
# names(filled_inout) <- "filling_inout"
names(sdm_tem) <- "SDM"
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
ecoregion_union <- ecoregion_union[!ecoregion_union$REALM1=="N/A",] #template for world map
ecoregion_union <- terra::vect(ecoregion_union)
ecoregion_union<- ecoregion_union[!ecoregion_union$REALM1=="Antarctica",] 
# unfilled <- terra::mask(unfilled,ecoregion_union) # mask to the surface of land area
# filled_onlyin <- terra::mask(filled_onlyin,ecoregion_union) # mask to the surface of land area
# filled_inout <- terra::mask(filled_inout,ecoregion_union) # mask to the surface of land area
sdm_tem <- terra::mask(sdm_tem,ecoregion_union) # mask to the surface of land area

#output 
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/tree_richness/')
dir.create(outputSpDir, recursive = T)
# writeRaster(x = unfilled,
#             file = paste0(outputSpDir,'/unfilling.tif'),overwrite=TRUE)
# writeRaster(x = filled_onlyin,
#             file = paste0(outputSpDir,'filling_onlyin.tif'),overwrite=TRUE)
# writeRaster(x = filled_inout,
#             file = paste0(outputSpDir,'filling_inout.tif'),overwrite=TRUE)   
writeRaster(x = sdm_tem,
            file = paste0(outputSpDir,'/sdm.tif'),overwrite=TRUE)  
