
##set up library
packs <- list("tidyverse","Hmisc","terra","hrbrthemes","sf","raster","car",
              "ggpubr")
lapply(packs, require, character.only = T)
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code")
# mean values of explanatory variables
mean_df_ana2 <- readRDS("mean_df_ana2.rds")
head(mean_df_ana2)

#change the column name
colnames(mean_df_ana2)[c(4,11,36)] <- c("Range_Unfilling","atr_median","realm")

# to see how range unfilling distributed: it follows pattern of lognormal distribution
p <- ggplot(aes(x=Range_Unfilling), data=mean_df_ana2)
p + geom_density()+xlim(0,37)
p <- ggplot(aes(x=log(Range_Unfilling)), data=mean_df_ana2)
p + geom_density() 
qqPlot(log(mean_df_ana2$Range_Unfilling))#normal distribution

# set 5 classes for RU====
sta_Range_Unfilling<- Hmisc::describe(mean_df_ana2$Range_Unfilling,quant=c(.05,.25,.50,.75,.95))
sd(mean_df_ana2$Range_Unfilling)
# 0-0.5,0.5-1,1-4,4-37,37 - +
# 0.5 as threshold, 4 is median value, 37 is the value of 0.95 quantile
mean_df_ana2$class_RU <-  ifelse(mean_df_ana2$Range_Unfilling<=0.5,1,
                                 ifelse(mean_df_ana2$Range_Unfilling<=1,2,
                                        ifelse(mean_df_ana2$Range_Unfilling<=4,3,
                                               ifelse(mean_df_ana2$Range_Unfilling<=37,4,5))))

mean_df_ana2$realm<- factor(mean_df_ana2$realm)
#percentage for class 3 to 5
1-(sum(mean_df_ana2$class_RU==1) + sum(mean_df_ana2$class_RU==2))/nrow(mean_df_ana2)
#percentage for class 1 and 2
class12 <- (sum(mean_df_ana2$class_RU==1) + sum(mean_df_ana2$class_RU==2))/nrow(mean_df_ana2)


data_means <- lapply(mean_df_ana2[,c(37,38)], mean)
data_means <- unlist(data_means)
data_means <- data.frame(data_means)

# boxplot for AIC and BIC====
plot0 <-
  boxplot(mean_df_ana2$AUC, mean_df_ana2$CBI1,
          at = c(1,2),
          names = c("AUC", "CBI")
  )
points(x = 1:nrow(data_means),                             # Add points to plot
       y = data_means$data_means,
       col = "red",
       pch = 16)
text(x = 1:nrow(data_means),                               # Add text to plot
     y = data_means$data_means-0.01,
     labels = paste("Mean:", round(data_means$data_means, 2)),
     col = "black")
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/annex')
dir.create(outputSpDir, recursive = T) # The directory you want to save the file in

# 1.Make the histogram for the RU distribution(global)====
plot1 <- mean_df_ana2 %>%
  ggplot(aes(x=Range_Unfilling)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  # ggtitle("Global tree disequilibrium status") +
  theme_ipsum() +
  geom_vline(linetype = "dotdash",aes(xintercept = quantile(Range_Unfilling,class12)
                                      ),color="red",size=1)+
  geom_vline(linetype = "dotdash",aes(xintercept = mean(Range_Unfilling)
  ),  size=1,color="purple")+
  geom_vline(linetype = "dotdash",aes(xintercept = median(Range_Unfilling)
  ),  size=1,color="blue")+
  theme( panel.background = element_rect(fill = "transparent",
         colour = NA_character_), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
         colour = NA_character_),
    plot.title = element_text(hjust = 0.5,size=20),axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x = element_text(hjust=0.5,size=15),
        axis.title.y = element_text(hjust=0.5,size=15))+
  labs(x="Rang Unfilling",y="Percent")+scale_x_continuous(breaks=c(0,10,20,30))+
  guides(colour = "none") +xlim(0,37)
  

# 2.potential species richness map====
ref_grid<-extent(-180, 180, -90, 90)
ref_grid<-raster(ref_grid)
res(ref_grid)<-0.0833333
values(ref_grid)<-1 #dummy values
projection(ref_grid)<-CRS("+proj=longlat +datum=WGS84 +no_defs+ellps=WGS84 +towgs84=0,0,0")
ref_grid <- rast(ref_grid)

filled <- rast("D:/quercus/Partage/Pengcheng/R/analyse/tree_richness/filling_onlyin.tif")
sdm <- rast("D:/quercus/Partage/Pengcheng/R/analyse/tree_richness/sdm.tif")
plot(sdm)
sdm[sdm==0] <- Inf 
species_filling <- filled/sdm
species_filling <- resample(species_filling,ref_grid,method="near")
# plot(species_filling)

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
#--- convert to data.frame ---#
species_filling <- as.data.frame(species_filling, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()


plot2 <- species_filling %>%
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = filling_onlyin))+
  coord_equal()+
  scale_fill_gradient2(low="darkgoldenrod1", high = "#336600",
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill"
  )+
  labs(fill= "Range filling"
      # ,title= "Global tree species range filling"
       ) +
theme_minimal() +
  theme( panel.background = element_rect(fill = "transparent",
                                         colour = NA_character_), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing plot outline
    plot.title = element_text(hjust = 0.5,size=20,face="bold"), axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    legend.text=element_text(size=13),
    legend.title = element_text(size=15)
   )+geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
  geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)


# 3.Make the barplot for the RU distribution at each realm====
plot3 <- ggplot(mean_df_ana2, aes(x= class_RU,  group=realm)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy = 0.1,fontface="bold"),
               y= ..prop.. ), stat= "count", hjust=0.3,vjust = -0.5,size=5) +
  labs(x="Range Unfilling",y = "Percent", fill="Class") +
  facet_grid(~realm) +
  scale_y_continuous(labels = scales::percent)+theme_minimal() +
  # ggtitle("Tree disequilibrium status in each realm") +
  theme(plot.title = element_text(hjust = 0.5,size=20 ,face="bold"),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x = element_text(hjust=0.5,size=15),
        axis.title.y = element_text(hjust=0.5,size=15),
        legend.text=element_text(size=13),
        legend.title = element_text(size=15),
        strip.text = element_text(
          size = 14,angle = 45,vjust=0.5))+
  scale_fill_discrete(labels=c('1: extremely low', '2: low',"3: moderate","4: high","5: extremely high"))

# 4.compare the mean RU in each realm using boxplot====
dataMean <- summarise(group_by(mean_df_ana2[mean_df_ana2$Range_Unfilling<=37,
                                            ], realm), MV = mean(Range_Unfilling))


plot4 <-
  ggboxplot(mean_df_ana2[mean_df_ana2$Range_Unfilling<=37,], 
            x = "realm", y = "Range_Unfilling", color = "realm", 
            add = "jitter",legend = "none")+
  stat_compare_means(method = "kruskal.test", label.y= 39)+        # Add global kruskal.test p-value
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.", hide.ns = TRUE)  +    # Pairwise comparison against all
  stat_summary(fun=mean, 
               aes(colour="mean",shape="mean"), 
               colour = "red",
               geom="point",size=3) +
  scale_shape_manual("", values=c("mean"="x")) +
  labs(y="Rang Unfilling",x="Realm")+
  ylim(0,40)+
  theme_minimal()+
  # ggtitle("Tree disequilibrium comparison") +
  theme(legend.position="none",
    plot.title = element_text(hjust = 0.5,size=20,face="bold"),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
    axis.text.x=element_text(angle = 45,hjust=0.5,size=13,vjust=0.7),
    axis.text.y=element_text(size=10),
    axis.title.x = element_text(hjust=0.5,size=15),
    axis.title.y = element_text(hjust=0.5,size=15),
    legend.text=element_text(size=13),
    legend.title = element_text(size=15)
    )+
  geom_hline(aes(yintercept = mean(mean_df_ana2$Range_Unfilling[mean_df_ana2$Range_Unfilling<=37])),
             linetype = 2,show.legend = T) +  # Add horizontal line at base mean
  geom_text(data = dataMean, aes(realm, MV, label = sprintf("%0.2f", round(MV, digits = 2))), 
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5)+
  geom_text(aes(x="Nearctic",label="6.24", y=8),
             size=5)

# kruskal-wilcox test for the whole dataset
# dataMean <- mean_df_ana2%>%
#   group_by(realm)%>%
#   summarise( MV = mean(Range_Unfilling))
# 
# plot5 <-
#   ggboxplot(mean_df_ana2[mean_df_ana2$Range_Unfilling,], 
#             x = "realm", y = "Range_Unfilling", color = "realm", 
#             add = "jitter",legend = "none") +
#   rotate_x_text(angle = 45)+
#   stat_compare_means(method = "kruskal.test", label.y= 10)+        # Add global kruskal.test p-value
#   stat_compare_means(label = "p.signif", method = "wilcox.test",
#                      ref.group = ".all.", hide.ns = TRUE)  +    # Pairwise comparison against all
#   stat_summary(fun=mean, 
#                aes(colour="mean",shape="mean"), 
#                colour = "red",
#                geom="point",size=3) +
#   scale_shape_manual("", values=c("mean"="x")) +
#   labs(y="Rang Unfilling",x="Realm")+
#   ylim(0,40)+
#   theme_minimal()+
#   # ggtitle("Tree disequilibrium comparison") +
#   theme(legend.position="none",
#         plot.title = element_text(hjust = 0.5,size=20,face="bold"),
#         panel.background = element_rect(fill = "transparent",
#                                         colour = NA_character_), # necessary to avoid drawing panel outline
#         panel.grid.major = element_blank(), # get rid of major grid
#         panel.grid.minor = element_blank(), # get rid of minor grid
#         plot.background = element_rect(fill = "transparent",
#                                        colour = NA_character_),axis.text.x=element_text(size=10),
#         axis.text.y=element_text(size=10),
#         axis.title.x = element_text(hjust=0.5,size=15),
#         axis.title.y = element_text(hjust=0.5,size=15),
#         legend.text=element_text(size=13),
#         legend.title = element_text(size=15))+
#   geom_hline(aes(yintercept = mean(mean_df_ana2$Range_Unfilling[mean_df_ana2$Range_Unfilling])),
#              linetype = 2,show.legend = T) +  # Add horizontal line at base mean
#   geom_text(data = dataMean, aes(realm, MV, label = sprintf("%0.2f", round(MV, digits = 2))), 
#             position = position_dodge(width = 0.8), size = 4, vjust = -0.5)+
#   geom_text(aes(x="Nearctic",label="10.7", y=10.7),
#             size=5)








#output
a<- ggarrange(ggarrange(plot1, plot4,ncol=1,nrow=2, widths = c(1, 1), labels = c("A", "C"),heights = c(4, 4)
                         ,align="v"),
               plot3,ncol=2,nrow = 1, 
              labels =c("","B"), widths=c(0.8,1),heights=c(7,7),
              font.label = list(size = 16)
                                                # Labels of the scatter plot
) 
b<- ggarrange(plot2)
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/RU_pattern')
dir.create(outputSpDir, recursive = T)
ggsave(filename = paste0(outputSpDir,"/samplegood.png", sep = "")
       ,plot= a,
       height = 10, width = 15, dpi = 1000)   # The directory you want to save the file in

# sta_RU_inout<- Hmisc::describe(mean_df_ana2$RU_inout, quant=c(.05,.25,.50,.75,.95))
# 0-0.5,0.5-1,1-2.5,2.5-25,25 - +
# 0.5 as threshold, 2.5 is median value, 25 is the value of 0.95 quantile
# mean_df_ana2$class_inout <-  ifelse(mean_df_ana2$RU_inout<=0.5,1,
#                                       ifelse(mean_df_ana2$RU_inout<=1,2,
#                                              ifelse(mean_df_ana2$RU_inout<=2.5,3,
#                                                     ifelse(mean_df_ana2$RU_inout<=25,4,5))))


# set colour ramp
colfunc<-colorRampPalette(c("forestgreen","yellow","red"))
RU_class_color <- colfunc(5) #for these 5 classes
# load world map
world <- map_data("world")
#world

##save a pdf document
pdf("D:/quercus/Partage/Pengcheng/R/analyse/RF_pdf_tif/RU.pdf", width=15, height=10)
# 5.plot species range unfilling across different realms====
# Step 1: Create the plot with R code
# plot RU which allow included area overlaped with alpha-hull
plot1 <- 
  ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = mean_df_ana2,
    aes(Long, Lat, 
        colour=factor(class_RU)),
    alpha = 0.5
  )   +
  # labs(title="Global species range unfilling distribution")+
  guides(col= guide_legend(title= "Class"))+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "transparent",
                                         colour = NA_character_), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing plot outline
         plot.title = element_text(hjust = 0.5,size=14), axis.text.x = element_blank(), 
         axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
         axis.title.x=element_blank(),axis.title.y=element_blank(),
         legend.text=element_text(size=13),
         legend.title = element_text(size=15)
  )+geom_sf(data = ecoregion,fill="transparent",color="black",size=1)+
  geom_sf_text(aes(label = REALM1), colour = "black",data = ecoregion,size=7)+
  scale_color_manual(values = c( RU_class_color),
    labels=c('1: extremely low', '2: low',"3: moderate","4: high","5: extremely high"))


# plot2<- 
#   ggplot() +
#   geom_map(
#     data = world, map = world,
#     aes(long, lat, map_id = region),
#     color = "white", fill = "lightgray", size = 0.1
#   ) +
#   geom_point(
#     data = mean_df_ana2,
#     aes(Long, Lat, 
#         colour=factor(class_inout)),
#     alpha = 0.5
#   ) + scale_colour_manual(values = c( RU_class_color))+
#   labs(title="Species Range Unfilling(Inout)") +
#   theme( 
#     plot.title = element_text(hjust = 0.5,size=16)) +
#   xlab("Longitude") +
#   ylab("Latitude")+
#   guides(col= guide_legend(title= "RU class"))

# step 2 save a png file
ggsave(filename = "D:/quercus/Partage/Pengcheng/R/analyse/RF_pdf_tif/RU_in.png",plot=plot1
       ,height = 10, width = 15, dpi = 1000)   # The directory you want to save the file in
# ggsave(filename = "D:/quercus/Partage/Pengcheng/R/analyse/RF_pdf_tif/RU_in_out.png",plot=plot2
# ,height = 10, width = 15, dpi = 320)   # The directory you want to save the file in

print(plot1) # let R know to print plot1
# print(plot2) # let R know to print plot2

# step3. finish the process
dev.off()

# plot occurrence data in global map(abies abla)====
occ <-readRDS ("D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc/Abies_alba/Abies_alba-occNonUnique.rds")
occ = unique (occ[,c('decimalLongitude','decimalLatitude')])
occ <- na.omit(occ)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

plot1 <- 
  ggplot(Europe,color = "white", fill = "lightgray", size = 0.1) +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  geom_point(
    data = occ,
    aes(decimalLongitude, decimalLatitude),
    alpha = 0.5
  )   +
  # labs(title="Global species range unfilling distribution")+
  # guides(col= guide_legend(title= "Class"))+
  theme_minimal() +
  theme( panel.background = element_rect(fill = "transparent",
                                         colour = NA_character_), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing plot outline
         plot.title = element_text(hjust = 0.5,size=14), axis.text.x = element_blank(), 
         axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
         axis.title.x=element_blank(),axis.title.y=element_blank(),
         legend.text=element_text(size=13),
         legend.title = element_text(size=15)
  )
