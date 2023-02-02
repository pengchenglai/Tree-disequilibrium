#set up working directory and library ====
setwd("D:/quercus/Partage/Pengcheng/R/HM_TC")
# Load packages
packs <- list("tidyverse", "raster", "sf", "terra","stars","rgdal","data.table","psych",
              "fasterize","patchwork","foreach",'doParallel',"fasterize")
lapply(packs, require, character.only = T)

# set path for temporary files
terraOptions(memfrac=0.9, tempdir = "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/tem")
rasterOptions(tmpdir="D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/tem")


#Rasterize dgg_tree_hm sf object====
###read the dataframe which contains geometry
dgg_tree_hm <- readRDS("data/dgg_tree_hm.rds")

#build a template for equal area projection
ref_grid<-extent(-180, 180, -90, 90)
ref_grid<-raster(ref_grid)
res(ref_grid)<-0.0833333
values(ref_grid)<-1 #dummy values
projection(ref_grid)<-CRS("+proj=longlat +datum=WGS84 +no_defs+ellps=WGS84 +towgs84=0,0,0")
ref_gridProj <- rast(ref_grid)
#init(ref_gridProj, prod(res(ref_gridProj)))

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
crs(ecoregion_union) <-"+proj=longlat +datum=WGS84 +no_defs+ellps=WGS84 +towgs84=0,0,0" 
ecoregion_union<- ecoregion_union[!ecoregion_union$REALM1=="Antarctica",] 
ecoregion<- st_as_sf(ecoregion_union)

###check required column names of dgg_tree_hm
names(dgg_tree_hm)

####select columns and rasterize these columns into equal area projection
col_num<-c(18:104,116:136,146:196)
col_names<-colnames(dgg_tree_hm[,col_num])
col_names <- col_names[1:159]
dgg_tree_hm = st_transform(dgg_tree_hm,
                     crs="+proj=longlat +datum=WGS84 +no_defs+ellps=WGS84 +towgs84=0,0,0")
                     # crs="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dgg_tree_hm <- terra::vect(dgg_tree_hm)

l=foreach(i=col_names,.packages = as.character("terra")) %do% {
  gc()
  a <-terra::rasterize(dgg_tree_hm, ref_gridProj, field=i)
  plot(a)
  writeRaster(a, filename=paste("raster/",i,".tif",sep=""), overwrite=TRUE)
  a
  gc()
}

#####for the convenience of adding columns with anthrome names 
ve <- c("11"="urban","12" ="Mixed settlements",
        "21"="Rice villages" ,"22" = "Irrigated villages",
        "23" ="Rainfed villages", "24" ="Pastoral villages",
        "31"= "Residential irrigated croplands",
        "32"="Residential rainfed croplands",
        "33"= "Populated croplands",
        "34"="Remote croplands",
        "41" ="Residential rangelands",
        "42"= "Populated rangelands",
        "43" = "Remote rangelands",
        "51" = "Residential woodlands",
        "52"= "Populated woodlands",
        "53"= "Remote woodlands",
        "54"= "Inhabited drylands",
        "61"= "Wild woodlands",
        "62" ="Wild drylands",
        "63" ="Ice",
        "70" = "NO DATA"
) 



#########1
####Plot raster layers##
#1.import anthrome raster layers====
an <- list.files(path="D:/quercus/Partage/Pengcheng/R/HM_TC/raster", full.names=TRUE, 
                 pattern="X.*.tif$")
###Sort input character name in numerical order
library(gtools)
anthrome <- rast(mixedsort(an))

#--- convert to data.frame ---#
anthrome <- as.data.frame(anthrome, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()

####transform numbers into character
anthromeok <- anthrome %>% mutate(anthrome10KB = ve[as.character(anthrome$X10000BC)])
anthromeok <- anthromeok %>% mutate(anthrome0 = ve[as.character(anthrome$X0AD)])
anthromeok <- anthromeok %>% mutate(anthrome1500 = ve[as.character(anthrome$X1500AD)])
anthromeok <- anthromeok %>% mutate(anthrome1850 = ve[as.character(anthrome$X1850AD)])
anthromeok <- anthromeok %>% mutate(anthrome1950= ve[as.character(anthrome$X1950AD)])
anthromeok <- anthromeok %>% mutate(anthrome2013= ve[as.character(anthrome$X2013AD)])
##names(anthromeok)
anthromeok<- anthromeok[,c(1,2,78:83)]


####import supporting information for plotting anthromes
###from Ellis et al.(2021) supporting materials
# Anthromes
anthrome_key <- tibble(
  anthrome = c(11, 12, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 51,
               52, 53, 54, 61, 62, 63, 70),
  class = factor(c('Urban', 'Mixed settlements', 'Rice villages', 'Irrigated villages',
                   'Rainfed villages', 'Pastoral villages', 'Residential irrigated croplands',
                   'Residential rainfed croplands', 'Populated croplands', 'Remote croplands',
                   'Residential rangelands', 'Populated rangelands', 'Remote rangelands',
                   'Residential woodlands', 'Populated woodlands', 'Remote woodlands',
                   'Inhabited drylands', 'Wild woodlands',
                   'Wild drylands', 'Ice', 'NO DATA'),
                 levels = c('Urban', 'Mixed settlements', 'Rice villages', 'Irrigated villages',
                            'Rainfed villages', 'Pastoral villages', 'Residential irrigated croplands',
                            'Residential rainfed croplands', 'Populated croplands', 'Remote croplands',
                            'Residential rangelands', 'Populated rangelands', 'Remote rangelands',
                            'Residential woodlands', 'Populated woodlands', 'Remote woodlands',
                            'Inhabited drylands', 'Wild woodlands',
                            'Wild drylands', 'Ice', 'NO DATA')),
  level = factor(c('Dense settlements', 'Dense settlements', 'Villages', 'Villages',
                   'Villages', 'Villages', 'Croplands', 'Croplands', 'Croplands', 'Croplands',
                   'Rangelands', 'Rangelands', 'Rangelands', 'Cultured', 'Cultured',
                   'Cultured', 'Cultured', 'Wildlands', 'Wildlands', 'Wildlands', 'NO DATA'),
                 levels = c('Dense settlements', 'Villages', 'Croplands',
                            'Rangelands', 'Cultured', 'Wildlands', 'NO DATA')),
  type = factor(c('Intensive', 'Intensive', 'Intensive', 'Intensive', 'Intensive', 'Intensive', 'Intensive', 'Intensive', 'Intensive',
                  'Intensive', 'Intensive', 'Intensive', 'Intensive', 'Cultured', 'Cultured','Cultured',
                  'Cultured', 'Wild', 'Wild', 'Wild', 'NO DATA'), levels = c('Intensive', 'Cultured', 'Wild', 'NO DATA'))
)

anthrome_class_color <- c('#A80000', '#FF0000', '#0070FF', '#00A9E6', '#A900E6',
                          '#FF73DF', '#00FFC5', '#E6E600', '#FFFF73', '#FFFFBE',
                          '#E69800', '#FFD37F', '#FFEBAF', '#38A800', '#A5F57A',
                          '#D3FFB2', '#D9BD75', '#DAF2EA', '#E1E1E1', '#FAFFFF', NA) %>%
  setNames(anthrome_key$class)


####plot anthromes for five designated years and save the results as png
####select columns and rasterize these columns 

#names(anthromeok)
col_num1 <- c(3:8)
col_names1 <- colnames(anthromeok[,col_num1])

###plot anthromes
##save a pdf document
pdf("png_pdf/anthromes/anthromes.pdf", width=15, height=10)
###plot anthromes in different years
l=foreach(i=col_names1) %do% {
  
  # Step 1: Create the plot with R code
  
  plot1= anthromeok %>% 
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = get(i))) +
    scale_fill_manual(values = c( anthrome_class_color)) +
    coord_equal() + labs(fill="Anthrome",caption = "Data source: Ellis et al.(2021)") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
  
  # Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/anthromes/", i,".png", sep = ""),plot=plot1
         ,height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  
  print(plot1)
  
  
}

#####plus overview of anthromes in different years 
#####data in a long format###
###increasing the number of rows and decreasing the number of columns
anthromeok_long_df <- anthromeok %>%
  pivot_longer(
    c(-x, -y),
    names_to = "time",
    values_to = "category"
  )

#####plot anthromes in 6 different years
plotover <- 
  ggplot() +
  geom_raster(data = anthromeok_long_df, aes(x = x, y = y, fill = category),stat="identity") +
  facet_wrap(time ~ .) +
  coord_equal() +
  scale_fill_manual(values = c( anthrome_class_color ))+
  labs(fill= "Anthrome" ,title= "Global anthromes distribtion",caption = "Data source: Ellis et al.(2021)") +
  theme( panel.background = element_rect(fill = "transparent",
                                         colour = NA_character_), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing plot outline
         plot.title = element_text(hjust = 0.5,size=12,face="bold"), axis.text.x = element_blank(), 
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(), axis.ticks.y = element_blank(),
         axis.title.x=element_blank(),axis.title.y=element_blank(),
         legend.text=element_text(size=13),
         legend.title = element_text(size=15)) +
geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
  geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
###add overview of anthromes
print(plotover)

###finish the process
dev.off()







#2.import  ajusted tree cover raster layers====

####import data in respect of difference of ajusted tree cover between different period
dif_atr<- list.files(path="D:/quercus/Partage/Pengcheng/R/HM_TC/raster", full.names=TRUE, 
                     pattern="dif.atr.*.tif$")
###Sort input character name in numerical order
library(gtools)
dif_atr <- rast(mixedsort(dif_atr))

#--- convert to data.frame ---#
dif_atr <- as.data.frame(dif_atr, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()

###select the columns
col_num2 <- c(3:17)
col_names2 <- colnames(dif_atr[,col_num2])

##save a pdf document
pdf("png_pdf/Difference_ajusted_tree_cover/Difference_ajusted_tree_cover.pdf", width=15, height=10)
####Save a png
l=foreach(i=col_names2) %do% {
### step1: code to plot
  plot2=dif_atr %>%
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = get(i)))+
  coord_equal()+
  scale_fill_gradient2(low="red4",mid = "white", high = "green4",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill"
  )+
  labs(fill= i,
 caption = "Based on data source: Hansen et al.(2013) & Ellis et al.(2021)") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)

# Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/Difference_ajusted_tree_cover/", i,".png", sep = "")
         ,plot= plot2,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
print(plot2)
}
###end of process
dev.off()





#3.import tree cover raster layers====
####import data in respect of difference of tree cover between different period
dif_tr<- list.files(path="D:/quercus/Partage/Pengcheng/R/HM_TC/raster", full.names=TRUE, 
                     pattern="dif.tr.*.tif$")
###Sort input character name in numerical order
library(gtools)
dif_tr <- rast(mixedsort(dif_tr))

#--- convert to data.frame ---#
dif_tr <- as.data.frame(dif_tr, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()

###select the columns
col_num3 <- c(3:17)
col_names3 <- colnames(dif_tr[,col_num3])

##save a pdf document
pdf("png_pdf/Difference_tree_cover/Difference_tree_cover.pdf", width=15, height=10)

####Save a png
l=foreach(i=col_names3) %do% {
  
  ### step1: code to plot
  plot3=dif_tr %>%
    ggplot()+
    geom_raster(aes(x = x, y = y, fill = get(i)))+
    coord_equal()+
    scale_fill_gradient2(low="red4",mid = "white", high = "green4",
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill"
    )+
    labs(fill= i,
         caption = "Based on data source: Hansen et al.(2013) & Ellis et al.(2021)") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
 geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
  
  # Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/Difference_tree_cover/", i,".png", sep = "")
         ,plot= plot3,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot3)
}
###end of process
dev.off()







#4.import hm raster layers====
####import data in respect of difference of hm between different period
dif_hm<- list.files(path="D:/quercus/Partage/Pengcheng/R/HM_TC/raster", full.names=TRUE, 
                    pattern="dif.hm.*.tif$")
###Sort input character name in numerical order
library(gtools)
dif_hm <- rast(mixedsort(dif_hm))

#--- convert to data.frame ---#
dif_hm <- as.data.frame(dif_hm, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()

###select the columns
col_num4 <- c(3:17)
col_names4 <- colnames(dif_hm[,col_num4])

###save a pdf file
pdf(file = "png_pdf/Difference_hm/Difference_hm.pdf", width=15, height=10)

####Save a png
l=foreach(i=col_names4) %do% {
  
  ### step1: code to plot
  plot4=dif_hm %>%
    ggplot()+
    geom_raster(aes(x = x, y = y, fill = get(i)))+
    coord_equal()+
    scale_fill_gradient2(low="blue",mid = "white", high = "red",
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill"
    )+
    labs(fill= i,
         caption = "Based on data source: Kennedy et al.(2019) & Ellis et al.(2021)") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
  
  # Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/Difference_hm/", i,".png", sep = "")
         ,plot= plot4,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot4)
}
###end of process
dev.off()





#5.import temperature raster layers====
####import data in respect of difference of temperature between different period and current temperature
tem<- list.files(path="D:/quercus/Partage/Pengcheng/R/HM_TC/raster", full.names=TRUE, 
                    pattern="tem.*.tif$")
###Sort input character name in numerical order
library(gtools)
tem <- rast(mixedsort(tem))

#--- convert to data.frame ---#
tem <- as.data.frame(tem, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
names(tem)

###select the columns
col_num5 <- c(4,5,7,8,9,11)
col_names5 <- colnames(tem[,col_num5])

###save a pdf file
pdf(file = "png_pdf/Temperature/Temperature.pdf", width=15, height=10)
####Save a png
l=foreach(i=col_names5) %do% {
  
  ### step1: code to plot
  plot5=tem %>%
    ggplot()+
    geom_raster(aes(x = x, y = y, fill = get(i)))+
    coord_equal()+
    scale_fill_gradient2(low="blue",mid = "white", high = "red",
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill"
    )+
    labs(fill= i,
         caption = "Based on data source: CHELSA & Pound et al.(2011)") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
  
  # Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/Temperature/", i,".png", sep = "")
         ,plot= plot5,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot5)
}
###end of process
dev.off()





#6.import precipitation raster layers====
####import data in respect of difference of precipitation  between different period and current temperature
pre<- list.files(path="D:/quercus/Partage/Pengcheng/R/HM_TC/raster", full.names=TRUE, 
                 pattern="pre.*.tif$")
###Sort input character name in numerical order
library(gtools)
pre <- rast(mixedsort(pre))

#--- convert to data.frame ---#
pre <- as.data.frame(pre, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
names(pre)

###select the columns
col_num6 <- c(4,5,7,8,9,11)
col_names6 <- colnames(pre[,col_num6])

###save a pdf file
pdf(file = "png_pdf/Precipitation/Precipitation.pdf", width=15, height=10)
####Save a png
l=foreach(i=col_names6) %do% {

  ### step1: code to plot
  plot6=pre %>%
    ggplot()+
    geom_raster(aes(x = x, y = y, fill = get(i)))+
    coord_equal()+
    scale_fill_gradient2(low="red",mid = "white", high = "blue",
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill"
    )+
    labs(fill= i,
         caption = "Based on data source: CHELSA & Pound et al.(2011)") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
  
  # Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/Precipitation/", i,".png", sep = "")
         ,plot= plot6,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
 print(plot6)
}

###end of process
dev.off()


####merge pdfs ====
qpdf::pdf_combine(input = c("png_pdf/anthromes/anthromes.pdf", "png_pdf/Difference_ajusted_tree_cover/Difference_ajusted_tree_cover.pdf",
                            "png_pdf/Difference_tree_cover/Difference_tree_cover.pdf","png_pdf/Difference_hm/Difference_hm.pdf",
                            "png_pdf/Precipitation/Precipitation.pdf","png_pdf/Temperature/Temperature.pdf"),
                  output = "png_pdf/summary.pdf")


#7.import hm layer====
hm <- rast("D:/quercus/Partage/Pengcheng/Summary/Data/Explaniable_data/human_actitives/HumanModificationIndex/lulc-human-modification-terrestrial-systems_geographic.tif")
hm <- terra::resample(hm,ref_gridProj)
names(hm) <- "hm"

plot(hm)
#--- convert to data.frame ---#
hm <- terra::as.data.frame(hm, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()



###save a pdf file
pdf(file = "png_pdf/hm/hm.pdf", width=15, height=10)
####Save a png

  ### step1: code to plot
  plot7=hm %>%
    ggplot()+
    geom_raster(aes(x = x, y = y, fill =hm))+
    coord_equal()+
    scale_fill_gradient2(low="blue",mid = "white", high = "red",
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill"
    )+
    labs(fill= "HM",
         caption = "Based on data source: CHELSA") +
    theme( panel.background = element_rect(fill = "transparent",
                                           colour = NA_character_), # necessary to avoid drawing panel outline
           panel.grid.major = element_blank(), # get rid of major grid
           panel.grid.minor = element_blank(), # get rid of minor grid
           plot.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing plot outline
           plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(),
           axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           axis.title.x=element_blank(),axis.title.y=element_blank(),
           legend.text=element_text(size=13),
           legend.title = element_text(size=15)) +
    geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
    geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)
  
  # Step 2: Call the ggsave command to save the plot
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/HM_TC/png_pdf/hm/","hm.png", sep = "")
         ,plot= plot7,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot7)
  
###end of process
dev.off()

