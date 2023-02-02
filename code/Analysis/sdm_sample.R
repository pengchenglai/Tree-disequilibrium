rm(list=ls())  ###start from scratch
##set up library
packs <- list("tidyverse",'doParallel','foreach','mapview',"sf","Hmisc","corrplot","randomForest"
              ,"pastecs","gtools","terra","raster","ggpubr","patchwork")
lapply(packs, require, character.only = T)

#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code")
# mean values of explanatory variables
mean_df_ana2 <- readRDS("mean_df_ana2.rds")

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

#get statistic map of SDMs/alpha hull
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

# get species id
(sp <-grep("Abies_alba",x=sta_mp))

            sta_map <- rast(sta_mp[sp])
            # plot(sta_map)
            # reminder: sta_map <- 10*pgr + gr 
            # made a new map named sta_map based on potential geographical range and geographical range
            # meaning of the values of new map: 0 represents this pixel is predicted as unsuitable from either
            # potential geographical range(pgr) and geographical range(gr) perspective; 1 represents this pixel
            # is only considered as suitable from gr; 10 represents this pixel is only thought as
            # suitable from pgr by excluding areas fall into 11; 11 represents this pixel is believed as suitable from 
            # both pgr and gr views. the ideal result is that we get 1 as less as possible
            # Note 1,10 might not exsit
            sdm <- sta_map==10 | sta_map==11
            # plot(sdm)
            sdm <- terra::resample(sdm,ref_gridProj,method="near")
            sdm <- terra::trim(sdm)
            #--- convert to data.frame ---#
            sdm <- as.data.frame(sdm, xy = TRUE) %>%
              #--- remove cells with NA for any of the layers ---#
              na.omit()
            
            plot1 <- sdm %>%
              ggplot()+
              geom_raster(aes(x = x, y = y, fill = factor(probalility)))+
              coord_equal()+
              labs(fill=NULL
                   # ,title= "Aies alba potential range"
                   ) +
              theme_minimal() +
              theme(  panel.background = element_rect(fill = "transparent",
                                                      colour = NA_character_), # necessary to avoid drawing panel outline
                      panel.grid.major = element_blank(), # get rid of major grid
                      panel.grid.minor = element_blank(), # get rid of minor grid
                      plot.background = element_rect(fill = "transparent",
                                                     colour = NA_character_),
                     plot.title = element_text(hjust = 0.5,size=12), axis.text.x = element_blank(), 
                     axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                     axis.title.x=element_blank(),axis.title.y=element_blank()
                     ,legend.position="none")+
              scale_fill_manual(values=c("grey","#3333FF"))
            
            
            alpha <- sta_map==11 |sta_map==1
            # plot(alpha)
            alpha <- terra::resample(alpha,ref_gridProj,method="near")
            alpha <- terra::trim(alpha)
            #--- convert to data.frame ---#
            alpha <- as.data.frame(alpha, xy = TRUE) %>%
              #--- remove cells with NA for any of the layers ---#
              na.omit()
            
            plot2 <- alpha %>%
              ggplot()+
              geom_raster(aes(x = x, y = y, fill = factor(probalility)))+
              coord_equal()+
              labs(fill=NULL
                   # ,title= "Aies alba geographic distribution"
                   ) +
              theme_minimal() +
              theme(  panel.background = element_rect(fill = "transparent",
                                                      colour = NA_character_), # necessary to avoid drawing panel outline
                      panel.grid.major = element_blank(), # get rid of major grid
                      panel.grid.minor = element_blank(), # get rid of minor grid
                      plot.background = element_rect(fill = "transparent",
                                                     colour = NA_character_),
                     plot.title = element_text(hjust = 0.5,size=12), axis.text.x = element_blank(), 
                     axis.ticks.x = element_blank(), axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.x=element_blank(),axis.title.y=element_blank(),
                     legend.text=element_text(size=13)
                     ,legend.position="none"
                     )+
              scale_fill_manual(values=c("grey","#FFCC00"))

             
             RU <- sta_map
             # plot(RU)
             RU <- terra::resample(RU,ref_gridProj,method="near")
             RU <- terra::trim(RU)
             #--- convert to data.frame ---#
             RU <- as.data.frame(RU, xy = TRUE) %>%
               #--- remove cells with NA for any of the layers ---#
               na.omit()
             
             plot3 <- RU %>%
               ggplot()+
               geom_raster(aes(x = x, y = y, fill = factor(probalility)))+
               coord_equal()+
               labs(fill=NULL
                    # ,title= "Aies alba potential range"
               ) +
               theme_minimal() +
               theme(  panel.background = element_rect(fill = "transparent",
                                                       colour = NA_character_), # necessary to avoid drawing panel outline
                       panel.grid.major = element_blank(), # get rid of major grid
                       panel.grid.minor = element_blank(), # get rid of minor grid
                       plot.background = element_rect(fill = "transparent",
                                                      colour = NA_character_),
                       plot.title = element_text(hjust = 0.5,size=12), axis.text.x = element_blank(), 
                       axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                       axis.title.x=element_blank(),axis.title.y=element_blank(),
                       legend.text=element_text(size=13)
                      )+
               scale_fill_manual(values=c("grey","#FFCC00","#3333FF","#006600"),
                                 labels=c("Absence","Presence(alpha-hull)",
                                          "Presence(SDM)",
                                          "Presence(both)"))
             
             patched <- plot2+plot1+ plot3
             patched<- patched + plot_annotation(tag_levels = 'A')&
               theme(plot.tag = element_text(size = 16))            
             a<- ggarrange( plot3,plot1, plot3,
                          labels =c("A","B","C"), 
                           font.label = list(size = 16),legend="top",common.legend = T)
                           


#output 
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/alpha_sdm_results_sample')
dir.create(outputSpDir, recursive = T)
ggsave(filename = paste0(outputSpDir,"/sample.png", sep = "")
       ,plot= patched,
       height = 10, width = 15, dpi = 1000)   # The directory you want to save the file in



