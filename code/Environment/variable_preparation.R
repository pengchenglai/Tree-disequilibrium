#set up working directory and library ====
setwd("D:/quercus/Partage/Pengcheng/R/HM_TC")
# Load packages
packs <- list("tidyverse", 
              "rgdal",'anthromes'
              ,"data.table","psych","fasterize","patchwork","foreach"
              ,"raster", "sf", "terra","stars")
lapply(packs, require, character.only = T)

#import anthrome data====
####import shape file(sf object) of anthromes
dgg <- read_sf("D:/quercus/Partage/Pengcheng/Summary/Data/Explaniable_data/human_actitives/Anthromes/12K_DGG_Full_Dataset/Anthromes-12k-DGG/an12_dgg_inputs/Anthromes-12k-DGG/an12_dgg_inputs.shp") %>%
  rename(land_area = land_ar, pot_vill = pot_vll, region_name = regn_nm) ##%>%
# mutate(region_name = if_else( # fix the region name
#   region_name == 'Latin America', 'Latin America and Caribbean',
#   region_name)) ####does not matter
####check names of region
##unique(dgg$region_name)
###Check statistics
##library(Hmisc)
##describe(dgg$land_area) ###96 km2 median value
##hist(dgg$land_area)  


###import anthromes data for different years
anthromes <- read.csv("D:/quercus/Partage/Pengcheng/Summary/Data/Explaniable_data/human_actitives/Anthromes/12K_DGG_Full_Dataset/an12_dgg_baseline.csv",header = TRUE, sep= ",")
# merge on common variable, here called 'id'
dgg <- merge(dgg, anthromes, by='id')
# perhaps save as shapefile again
#shapefile(dgg, "Anthromes-12k-DGG/an12_dgg_inputs/Anthromes-12k-DGG/merged_dgg.shp")
#class(dgg)


#####FUNCTION TO EXTRACT RASTER LAYER VALUE BASED ON GEOMETRY OF PLOYGONS====
#' Extract raster data with a DGG shapefile
#'
#' Given a stars data object and shapefile, extracts the relevant data using
#' exactextractr::exact_extract(). Stars inputs should be in raster format with
#' x and y dimensions. This function is planned for deprecation whenever direct
#' support for exact_extract() is better supported via stars::aggregate().
#' @param dat The raster data to extract.
#' @param dgg Shapefile containing the DGG boundaries to use for the extraction.
#' @param var The variable to extract.
#' @param fun The function to pass to exactextractr::exact_extract().
#' @param progress Display the exactextractr::exact_extract() progress bar.
#'
#' @return
#' @export
#'
dgg_extract <- function(dat, dgg, var = NULL, fun, progress = TRUE) {
  if(is.null(var)) {
    exactextractr::exact_extract(as(dat, 'Raster'), dgg, fun, progress = progress, force_df = TRUE) %>%
      #      rename_with(str_remove, pattern = paste0(fun, '.')) %>%
      dplyr::mutate(geometry = dgg$geom) %>%
      sf::st_as_sf() %>%
      stars::st_as_stars()
  } else {
    exactextractr::exact_extract(as(dat[var], 'Raster'), dgg, fun, progress = progress) %>%
      dplyr::mutate(geometry = dgg$geom) %>%
      sf::st_as_sf() %>%
      stars::st_as_stars() %>%
      merge(name = 'time') %>%
      stars::st_set_dimensions('time',
                               stars::st_get_dimension_values(dat, 'time')) %>%
      setNames(var)
  }
}


#import tree cover and human modification raster layers ====
tr <- rast("C:/Users/Utilisateur/Desktop/Internship/GIS/Data2010/Tree cover/HansenTC.tif")
hm <- rast("S:/BD_SIG/occup_sol/HumanModificationIndex/lulc-human-modification-terrestrial-systems_geographic.tif")
mio_pre<- rast("S:/Partage/Pengcheng/Summary/Data/Explaniable_data/Climate_anomaly/Late Miocene climate_Tortonian/Miocene/Miocene/mio_prec_360.tif")
mio_tem<- rast("S:/Partage/Pengcheng/Summary/Data/Explaniable_data/Climate_anomaly/Late Miocene climate_Tortonian/Miocene/Miocene/mio_temp_10.tif")
lgm_tem<- rast("S:/Partage/Pengcheng/Summary/Data/Explaniable_data/Climate_anomaly/CHELSA-TraCE21k_LGM/CHELSA_TraCE21k_bio01_-200_V1.0.tif")
lgm_pre<- rast("S:/Partage/Pengcheng/Summary/Data/Explaniable_data/Climate_anomaly/CHELSA-TraCE21k_LGM/CHELSA_TraCE21k_bio12_-200_V1.0.tif")
cur_tem<- rast("S:/Partage/Pengcheng/Summary/Data/Explaniable_data/Climate_anomaly/Current climate variables/CHELSA_bio1_1981-2010_V.2.1.tif")
cur_pre<- rast("S:/Partage/Pengcheng/Summary/Data/Explaniable_data/Climate_anomaly/Current climate variables/CHELSA_bio12_1981-2010_V.2.1.tif")


###correlation analysis between HM and tree cover
#tree<- resample(tr,hm,method="bilinear")
##library(Hmisc)
#res <- rcorr(as.matrix(c(tree,hm)),type ="spearman")
#res
# Insignificant correlations are leaved blank
#library(corrplot)
#corrplot(res$r, type="upper", order="original", 
#         p.mat = res1$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
#         number.cex = 0.5,tl.col="black",tl.srt=70)

#extract data based on anthromes_dgg====
####EXTRACT RASTER LAYER VALUE BASED ON GEOMETRY OF PLOYGONS
tr_star <- dgg_extract(tr,dgg, fun= c("median","mode","mean","sum","min","max"))
hm_star <- dgg_extract(hm,dgg, fun= c("median","mode","mean","sum","min","max"))
mio_pre_star <- dgg_extract(mio_pre,dgg, fun= c("median","mean"))
mio_tem_star <- dgg_extract(mio_tem,dgg, fun= c("median","mean"))
lgm_tem_star <- dgg_extract(lgm_tem,dgg, fun= c("median","mean"))
lgm_pre_star <- dgg_extract(lgm_pre,dgg, fun= c("median","mean"))
cur_pre_star<- dgg_extract(cur_pre,dgg, fun= c("median","mean"))
cur_tem_star<- dgg_extract(cur_tem,dgg, fun= c("median","mean"))
###
##ONLY TAKE MEDIAN VALUES AS MATTER OF FACT

##set star objects as sf objects in order to merge
tr_sf <- st_as_sf(tr_star)
hm_sf <- st_as_sf(hm_star)

##rename the column names of obejcts
names(tr_sf) <- paste0("tr_", names(tr_sf))
names(hm_sf) <- paste0("hm_", names(hm_sf))

#####set star objects as sf objects in order to merge
mio_pre_sf <- st_as_sf(mio_pre_star)
mio_tem_sf <- st_as_sf(mio_tem_star)
lgm_pre_sf <- st_as_sf(lgm_pre_star)
lgm_tem_sf <- st_as_sf(lgm_tem_star)
cur_pre_sf <- st_as_sf(cur_pre_star)
cur_tem_sf <- st_as_sf(cur_tem_star)

##rename the column names of obejcts
names(mio_pre_sf) <- paste0("mio_pre_", names(mio_pre_sf))
names(mio_tem_sf) <- paste0("mio_tem_", names(mio_tem_sf))
names(lgm_pre_sf) <- paste0("lgm_pre_", names(lgm_pre_sf))
names(lgm_tem_sf) <- paste0("lgm_tem_", names(lgm_tem_sf))
names(cur_pre_sf) <- paste0("cur_pre_", names(cur_pre_sf))
names(cur_tem_sf) <- paste0("cur_tem_", names(cur_tem_sf))


###Join these three sf objects together
dgg_fac<- cbind(tr_sf,hm_sf,dgg,mio_pre_sf,mio_tem_sf,
                lgm_pre_sf,lgm_tem_sf,cur_pre_sf,cur_tem_sf)
####store the sf object in case of crash
####dgg_fac includes info considering tree cover(2010),human modification(2016),anthromes
###These info all stored in dgg formate which is equal-area system covering earth surface
##dgg_facok <- readRDS("C:/Users/Utilisateur/Desktop/Internship/Anthromes/database/Anthromes-12k-DGG/an12_dgg_inputs/Anthromes-12k-DGG/dgg_facok.rds")



###Start the operation
####add name for each anthromes catergory to respective new columns
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


dgg_facok <- dgg_fac %>% mutate(anthrome2010AD = ve[as.character(dgg_fac$X2010AD)])
#rm(dgg_fac)
#gc()
dgg_facok <- dgg_facok %>% mutate(anthrome2016AD = ve[as.character(dgg_fac$X2016AD)])
#names(dgg_facok)

#start from the calculus
# dgg_facok <- readRDS("D:/quercus/Partage/Pengcheng/R/HM_TC/data/dgg_tree_hm.rds")
#plot statistics of tc,hm,atr====
# Violin basic
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
####plot tree cover for each anthromes category in the year 2010
###ggplot_violin
#dgg_facok <- data.table(dgg_facok)
plot1 <- dgg_facok %>%
  ggplot( aes(x=fct_reorder(anthrome2010AD,tr_median), y=tr_median, fill= anthrome2010AD)) +
  # geom_violin(width=1,draw_quantiles = c(.25, .50, .75),show.legend = F) +
  geom_violin(scale = "width",draw_quantiles = c(.25, .50, .75))+
  theme(plot.title = element_text(hjust = 0.5,size=20 ,face="bold"),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(size=13,angle=90,vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x = element_text(hjust=0.5,size=15),
        axis.title.y = element_text(hjust=0.5,size=15),
        legend.text=element_text(size=13),
        legend.title = element_text(size=15),
        legend.position = "none") +
  # ggtitle("Tree cover of each anthromes category ") +
  xlab("Anthrome")+
  ylab("Tree cover")
###See how tree cover distributed 
###hist(dgg_facok$tree_median)


####plot statistics of human modification for each anthromes category in the year 2016
plot2 <- dgg_facok %>%
  ggplot( aes(x=reorder(anthrome2016AD,hm_median), y=hm_median, fill= anthrome2016AD)) +
  # geom_violin(width=1,draw_quantiles = c(.25, .50, .75),show.legend = F) +
  geom_violin(scale = "width",draw_quantiles = c(.25, .50, .75))+
  theme(plot.title = element_text(hjust = 0.5,size=20 ,face="bold"),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(size=13,angle=90,vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x = element_text(hjust=0.5,size=15),
        axis.title.y = element_text(hjust=0.5,size=15),
        legend.text=element_text(size=13),
        legend.title = element_text(size=15),
        legend.position = "none" ) +
  # ggtitle("HM of each anthromes category ") +
  xlab("Anthrome")+
  ylab("Human modification")
###See how tree cover distributed 
###hist(dgg_facok$hm_median)

library(data.table)
####transform dgg_facok in order to calculate statistics
Stadgg_fac <- data.table(dgg_facok)
##saveRDS(dgg_facok, "Anthromes-12k-DGG/an12_dgg_inputs/Anthromes-12k-DGG/dgg_facok.rds")
#dgg_facok <- readRDS("Anthromes-12k-DGG/an12_dgg_inputs/Anthromes-12k-DGG/dgg_facok.rds")

#### Get statistics of tree cover based on anthromes
Stadgg_tree <- Stadgg_fac[,psych::describe(tr_median,na.rm= TRUE, quant=c(.05,.25,.75,.95)),by=anthrome2010AD] 


####PLOT Median value of tree cover for each anthromes category(2010)
plot3 <- Stadgg_tree %>%
  ggplot(aes(x = reorder(anthrome2010AD,median) , y= median)) +
  geom_bar( stat = "identity")+
  labs(x="Anthrome",y="Tree cover")+
  theme(
        plot.title = element_text(hjust = 0.5,size=20 ,face="bold"),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(size=13,angle=90,vjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x = element_text(hjust=0.5,size=15),
        axis.title.y = element_text(hjust=0.5,size=15),
        legend.text=element_text(size=13),
        legend.title = element_text(size=15),
        legend.position = "none")


#### Get statistics of HM based on anthromes
Stadgg_hm <- Stadgg_fac[,psych::describe(hm_median,na.rm= TRUE, quant=c(.05,.25,.75,.95)),by=anthrome2016AD]



####PLOT Median value of HM for each anthromes category
plot4 <- Stadgg_hm %>%
  ggplot(aes(x = reorder(anthrome2016AD,median) , y= median)) +
  geom_bar( stat = "identity")+
labs(x="Anthrome",y="Human modification")+
  theme(
    plot.title = element_text(hjust = 0.5,size=20 ,face="bold"),
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    axis.text.x=element_text(size=13,angle=90,vjust=0.5),
    axis.text.y=element_text(size=10),
    axis.title.x = element_text(hjust=0.5,size=15),
    axis.title.y = element_text(hjust=0.5,size=15),
    legend.text=element_text(size=13),
    legend.title = element_text(size=15),
    legend.position = "none")

# tree cover
patched <- plot1+plot3
patched<- patched + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size = 16))
#output ====
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/annex')
dir.create(outputSpDir, recursive = T)
ggsave(filename = paste0(outputSpDir,"/treecover.png", sep = "")
       ,plot= patched,
       height = 10, width = 15, dpi = 1000)   # The directory you want to save the file in
# hm
patched <- plot2+plot4
patched<- patched + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size = 16))
ggsave(filename = paste0(outputSpDir,"/hm.png", sep = "")
       ,plot= patched,
       height = 10, width = 15, dpi = 1000)

####join the median info of tree cover and human modification to the dgg_facok
dgg_tree<- right_join(dgg_facok,Stadgg_tree[,c("anthrome2010AD","tree_median2010")],by="anthrome2010AD")
dgg_tree_hm <- right_join(dgg_tree,Stadgg_hm[,c("anthrome2016AD","hm_median2016")],by="anthrome2016AD")

#####create new columns in statistics table for the sake of clearance
###supposing median statistic does not change so much
###the values(anthrome names) are the same
Stadgg_tree <- Stadgg_tree %>%  
  mutate(tree_median10000BC =tree_median2010,
         tree_median0AD=tree_median2010,
         tree_median1500AD=tree_median2010,
         tree_median1850AD =tree_median2010,
         tree_median1950AD =tree_median2010,
         tree_median2013AD =tree_median2010,
         tree_median2017AD =tree_median2010,
         anthrome10000BC =anthrome2010AD,
         anthrome0AD=anthrome2010AD,
         anthrome1500AD=anthrome2010AD,
         anthrome1850AD =anthrome2010AD,
         anthrome1950AD =anthrome2010AD,
         anthrome2013AD =anthrome2010AD,
         anthrome2016AD =anthrome2010AD,
         anthrome2017AD =anthrome2010AD)

Stadgg_hm <- Stadgg_hm %>%
  mutate(hm_median10000BC =hm_median2016,
         hm_median0AD=hm_median2016,
         hm_median1500AD=hm_median2016,
         hm_median1850AD =hm_median2016,
         hm_median1950AD =hm_median2016,
         hm_median2013AD =hm_median2016,
         hm_median2017AD =hm_median2016,
         absence_hm =(1-hm_median2016),
         anthrome10000BC =anthrome2016AD,
         anthrome0AD=anthrome2016AD,
         anthrome1500AD=anthrome2016AD,
         anthrome1850AD =anthrome2016AD,
         anthrome1950AD =anthrome2016AD,
         anthrome2013AD =anthrome2016AD,
         anthrome2017AD =anthrome2016AD) 

###Plot adjusted treecove
Stadgg_ATC <-left_join(Stadgg_hm %>% dplyr::select(absence_hm,anthrome2016AD),
                       Stadgg_tree %>% dplyr::select(tree_median2010,anthrome2016AD),by= "anthrome2016AD")
Stadgg_ATC$adjusted_treecover <- Stadgg_ATC$absence_hm*Stadgg_ATC$tree_median2010
Stadgg_ATC %>%
  ggplot(aes(x = reorder(anthrome2016AD,adjusted_treecover) , y= adjusted_treecover)) +
  geom_bar( stat = "identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###add new columns to match in order to join ajusted tree cover
Stadgg_ATC  <- Stadgg_ATC  %>%  
  mutate(Atree_median10000BC =adjusted_treecover,
         Atree_median0AD=adjusted_treecover,
         Atree_median1500AD=adjusted_treecover,
         Atree_median1850AD =adjusted_treecover,
         Atree_median1950AD =adjusted_treecover,
         Atree_median2013AD =adjusted_treecover,
         Atree_median2017AD =adjusted_treecover,
         anthrome10000BC =anthrome2016AD,
         anthrome0AD=anthrome2016AD,
         anthrome1500AD=anthrome2016AD,
         anthrome1850AD =anthrome2016AD,
         anthrome1950AD =anthrome2016AD,
         anthrome2013AD =anthrome2016AD,
         anthrome2017AD =anthrome2016AD)


###create new columns in dgg_tree_hm in order to match different period 
dgg_tree_hm <- dgg_tree_hm %>% mutate(anthrome10000BC = ve[as.character(dgg_tree_hm$X10000BC)],
                                      anthrome0AD=ve[as.character(dgg_tree_hm$X0AD)],
                                      anthrome1500AD=ve[as.character(dgg_tree_hm$X1500AD)],
                                      anthrome1850AD=ve[as.character(dgg_tree_hm$X1850AD)],
                                      anthrome1950AD=ve[as.character(dgg_tree_hm$X1950AD)],
                                      anthrome2013AD=ve[as.character(dgg_tree_hm$X2013AD)],
                                      anthrome2017AD=ve[as.character(dgg_tree_hm$X2017AD)])

#join statistics of tc,atc and hm to dgg_tree_hm(database)====
###join columns to estimate tree cover,adjusted tree cover and hm in each period
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome10000BC","hm_median10000BC")],by="anthrome10000BC")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome0AD","hm_median0AD")],by="anthrome0AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome1500AD","hm_median1500AD")],by="anthrome1500AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome1850AD","hm_median1850AD")],by="anthrome1850AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome1950AD","hm_median1950AD")],by="anthrome1950AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome2013AD","hm_median2013AD")],by="anthrome2013AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_hm[,c("anthrome2017AD","hm_median2017AD")],by="anthrome2017AD")
####calculate tree cover in different years
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome10000BC","tree_median10000BC")],by="anthrome10000BC")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome0AD","tree_median0AD")],by="anthrome0AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome1500AD","tree_median1500AD")],by="anthrome1500AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome1850AD","tree_median1850AD")],by="anthrome1850AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome1950AD","tree_median1950AD")],by="anthrome1950AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome2013AD","tree_median2013AD")],by="anthrome2013AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_tree[,c("anthrome2017AD","tree_median2017AD")],by="anthrome2017AD")
###calculate adjusted tree cover in different years
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome10000BC","Atree_median10000BC")],by="anthrome10000BC")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome0AD","Atree_median0AD")],by="anthrome0AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome1500AD","Atree_median1500AD")],by="anthrome1500AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome1850AD","Atree_median1850AD")],by="anthrome1850AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome1950AD","Atree_median1950AD")],by="anthrome1950AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome2013AD","Atree_median2013AD")],by="anthrome2013AD")
dgg_tree_hm <- right_join(dgg_tree_hm,Stadgg_ATC[,c("anthrome2017AD","Atree_median2017AD")],by="anthrome2017AD")

#calulate the data between different years(periods) ====
###1====
####calcaulte difference of tree cover between different periods(present-past)
dgg_tree_hm$dif_tr_2013_10kBC <-  dgg_tree_hm$tree_median2013AD-dgg_tree_hm$tree_median10000BC
dgg_tree_hm$dif_tr_2013_0 <-  dgg_tree_hm$tree_median2013AD-dgg_tree_hm$tree_median0AD
dgg_tree_hm$dif_tr_2013_1500 <-  dgg_tree_hm$tree_median2013AD-dgg_tree_hm$tree_median1500AD
dgg_tree_hm$dif_tr_2013_1850 <-  dgg_tree_hm$tree_median2013AD-dgg_tree_hm$tree_median1850AD
dgg_tree_hm$dif_tr_2013_1950 <-  dgg_tree_hm$tree_median2013AD-dgg_tree_hm$tree_median1950AD

####calcaulte difference of adjusted tree cover between different periods(present-past)
dgg_tree_hm$dif_atr_2013_10kBC <-  dgg_tree_hm$Atree_median2013AD-dgg_tree_hm$Atree_median10000BC
dgg_tree_hm$dif_atr_2013_0 <-  dgg_tree_hm$Atree_median2013AD-dgg_tree_hm$Atree_median0AD
dgg_tree_hm$dif_atr_2013_1500 <-  dgg_tree_hm$Atree_median2013AD-dgg_tree_hm$Atree_median1500AD
dgg_tree_hm$dif_atr_2013_1850<-  dgg_tree_hm$Atree_median2013AD-dgg_tree_hm$Atree_median1850AD
dgg_tree_hm$dif_atr_2013_1950 <-  dgg_tree_hm$Atree_median2013AD-dgg_tree_hm$Atree_median1950AD

####calcaulte difference of hm between different periods(present-past)
dgg_tree_hm$dif_hm_2013_10kBC <-  dgg_tree_hm$hm_median2013AD-dgg_tree_hm$hm_median10000BC
dgg_tree_hm$dif_hm_2013_0 <-  dgg_tree_hm$hm_median2013AD-dgg_tree_hm$hm_median0AD
dgg_tree_hm$dif_hm_2013_1500 <-  dgg_tree_hm$hm_median2013AD-dgg_tree_hm$hm_median1500AD
dgg_tree_hm$dif_hm_2013_1850<-  dgg_tree_hm$hm_median2013AD-dgg_tree_hm$hm_median1850AD
dgg_tree_hm$dif_hm_2013_1950 <-  dgg_tree_hm$hm_median2013AD-dgg_tree_hm$hm_median1950AD



###2====
####calcaulte difference of tree cover between different periods(globalization - past)
dgg_tree_hm$dif_tr_1950_10kBC <-  dgg_tree_hm$tree_median1950AD-dgg_tree_hm$tree_median10000BC
dgg_tree_hm$dif_tr_1950_0 <-  dgg_tree_hm$tree_median1950AD-dgg_tree_hm$tree_median0AD
dgg_tree_hm$dif_tr_1950_1500 <-  dgg_tree_hm$tree_median1950AD-dgg_tree_hm$tree_median1500AD
dgg_tree_hm$dif_tr_1950_1850 <-  dgg_tree_hm$tree_median1950AD-dgg_tree_hm$tree_median1850AD

####calcaulte difference of adjusted tree cover between different periods(globalization-past)
dgg_tree_hm$dif_atr_1950_10kBC <-  dgg_tree_hm$Atree_median1950AD-dgg_tree_hm$Atree_median10000BC
dgg_tree_hm$dif_atr_1950_0 <-  dgg_tree_hm$Atree_median1950AD-dgg_tree_hm$Atree_median0AD
dgg_tree_hm$dif_atr_1950_1500 <-  dgg_tree_hm$Atree_median1950AD-dgg_tree_hm$Atree_median1500AD
dgg_tree_hm$dif_atr_1950_1850<-  dgg_tree_hm$Atree_median1950AD-dgg_tree_hm$Atree_median1850AD

####calcaulte difference of hm between different periods(globalization-past)
dgg_tree_hm$dif_hm_1950_10kBC <-  dgg_tree_hm$hm_median1950AD-dgg_tree_hm$hm_median10000BC
dgg_tree_hm$dif_hm_1950_0 <-  dgg_tree_hm$hm_median1950AD-dgg_tree_hm$hm_median0AD
dgg_tree_hm$dif_hm_1950_1500 <-  dgg_tree_hm$hm_median1950AD-dgg_tree_hm$hm_median1500AD
dgg_tree_hm$dif_hm_1950_1850<-  dgg_tree_hm$hm_median1950AD-dgg_tree_hm$hm_median1850AD



###3====
####calcaulte difference of tree cover between different periods(Industrial revolution - past)
dgg_tree_hm$dif_tr_1850_10kBC <-  dgg_tree_hm$tree_median1850AD-dgg_tree_hm$tree_median10000BC
dgg_tree_hm$dif_tr_1850_0 <-  dgg_tree_hm$tree_median1850AD-dgg_tree_hm$tree_median0AD
dgg_tree_hm$dif_tr_1850_1500 <-  dgg_tree_hm$tree_median1850AD-dgg_tree_hm$tree_median1500AD

####calcaulte difference of adjusted tree cover between different periods(Industrial revolution - past)
dgg_tree_hm$dif_atr_1850_10kBC <-  dgg_tree_hm$Atree_median1850AD-dgg_tree_hm$Atree_median10000BC
dgg_tree_hm$dif_atr_1850_0 <-  dgg_tree_hm$Atree_median1850AD-dgg_tree_hm$Atree_median0AD
dgg_tree_hm$dif_atr_1850_1500 <-  dgg_tree_hm$Atree_median1850AD-dgg_tree_hm$Atree_median1500AD

####calcaulte difference of hm between different periods(Industrial revolution - past)
dgg_tree_hm$dif_hm_1850_10kBC <-  dgg_tree_hm$hm_median1850AD-dgg_tree_hm$hm_median10000BC
dgg_tree_hm$dif_hm_1850_0 <-  dgg_tree_hm$hm_median1850AD-dgg_tree_hm$hm_median0AD
dgg_tree_hm$dif_hm_1850_1500 <-  dgg_tree_hm$hm_median1850AD-dgg_tree_hm$hm_median1500AD



###4====
####calcaulte difference of tree cover between different periods(columbian exchange - past)
dgg_tree_hm$dif_tr_1500_10kBC <-  dgg_tree_hm$tree_median1500AD-dgg_tree_hm$tree_median10000BC
dgg_tree_hm$dif_tr_1500_0 <-  dgg_tree_hm$tree_median1500AD-dgg_tree_hm$tree_median0AD

####calcaulte difference of adjusted tree cover between different periods(columbian exchange - past)
dgg_tree_hm$dif_atr_1500_10kBC <-  dgg_tree_hm$Atree_median1500AD-dgg_tree_hm$Atree_median10000BC
dgg_tree_hm$dif_atr_1500_0 <-  dgg_tree_hm$Atree_median1500AD-dgg_tree_hm$Atree_median0AD

####calcaulte difference of hm between different periods(columbian exchange - past)
dgg_tree_hm$dif_hm_1500_10kBC <-  dgg_tree_hm$hm_median1500AD-dgg_tree_hm$hm_median10000BC
dgg_tree_hm$dif_hm_1500_0 <-  dgg_tree_hm$hm_median1500AD-dgg_tree_hm$hm_median0AD


###5====
####calcaulte difference of tree cover between different periods(agricurtural/pastoral transformation - past)
dgg_tree_hm$dif_tr_0_10kBC <-  dgg_tree_hm$tree_median0AD-dgg_tree_hm$tree_median10000BC

####calcaulte difference of adjusted tree cover between different periods(columbian exchange - past)
dgg_tree_hm$dif_atr_0_10kBC <-  dgg_tree_hm$Atree_median0AD-dgg_tree_hm$Atree_median10000BC

####calcaulte difference of hm between different periods(columbian exchange - past)
dgg_tree_hm$dif_hm_0_10kBC <-  dgg_tree_hm$hm_median0AD-dgg_tree_hm$hm_median10000BC

###6====
##calcaulte difference of precipitation between different periods(miocene  - lgm - current)
dgg_tree_hm$mio_pre_lgm<-dgg_tree_hm$mio_pre_median-dgg_tree_hm$lgm_pre_median
dgg_tree_hm$mio_pre_cur<-dgg_tree_hm$mio_pre_median-dgg_tree_hm$cur_pre_median

###7====
##calcaulte difference of temperature between different periods(miocene  - lgm - current)
dgg_tree_hm$mio_tem_lgm<-(dgg_tree_hm$mio_tem_median/10)-dgg_tree_hm$lgm_tem_median
dgg_tree_hm$mio_tem_cur<-(dgg_tree_hm$mio_tem_median/10)-dgg_tree_hm$cur_tem_median

###8
##calcaulte difference of precipitation between different periods(lgm - current)
dgg_tree_hm$lgm_pre_cur<-dgg_tree_hm$lgm_pre_median-dgg_tree_hm$cur_pre_median

###9
##calcaulte difference of temperature between different periods(lgm - current)
dgg_tree_hm$lgm_tem_cur<-dgg_tree_hm$lgm_tem_median-dgg_tree_hm$cur_tem_median



###remove all other columns contain "_geometry" pattern in the names
dgg_tree_hm[, grep("_geometry",colnames(dgg_tree_hm),value=T)]<-NULL
###save data as rds format
saveRDS(dgg_tree_hm, "Anthromes-12k-DGG/an12_dgg_inputs/Anthromes-12k-DGG/dgg_tree_hm.rds")