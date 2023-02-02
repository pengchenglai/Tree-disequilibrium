rm(list=ls())  ###start from scratch
#set up working directory ====
setwd("D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar")
##set up library
packs <- list("tidyverse",'doParallel','foreach','mapview',"sf","Hmisc","corrplot","randomForest"
              ,"pastecs","gtools","terra","lme4","lmerTest","sjPlot","car","ggeffects","hrbrthemes",
              "ggpubr","gridExtra")
lapply(packs, require, character.only = T)

# get individual species statistics and compile them
df_an <- list.files(pattern = 'staCen.rds$',
                     recursive = T)
spNam = strsplit(df_an,'/')
spNam = lapply (spNam, function (x) x[1])
spNam = unlist (spNam)

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
spName <- c(spName,spName2,spName3)

# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =12
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

#get right species id via reduction of problematic species
spname <- setdiff(spNam,spName)
sptodo = foreach(i=spname) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = df_an)})}
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

# get database ====
df_ana = lapply(df_an[sptod], readRDS)
df_ana <-dplyr::bind_rows(df_ana)

# table(df_ana$realm1) 

# names(df_ana)
# select columns of interests
# sorry for being bit lazy
a <- c(1,3,5,8,9,10,11,17,18,19,20,40,46,51,56,60,64,68,71,74,77,79,81,83,84,85,86,87:93,95)
a <- a+2
a <- c(1,2,a) # I have add columns which contain coordinates of cenriod
df_ana2 <- df_ana[,a]
df_ana2 <-na.omit(df_ana2) 
df_ana2$Atree_2013_bias <- df_ana2$tr_median * (1-df_ana2$hm_median)
# names(df_ana2)
# reorder the columns
a<- c(3:7,36,10:27,8,9,28:35,1:2)
a <- a+2
a <- c(1,2,a) # I have add columns which contain coordinates of cenriod
df_ana2 <- df_ana2[,a]
df_ana2 <- df_ana2[df_ana2$CBI1>=0.8,]
# a= lapply(df_ana2[,c(37:38)], mean)

# mean statistics ====
mean_df_ana2 <- df_ana2[df_ana2$sta=="mean",]
# median statistics ====
median_df_ana2 <- df_ana2[df_ana2$sta=="median",]
# mean(df_ana2$AUC)
# mean(df_ana2$CBI1)


# set working directory
setwd("D:/quercus/Partage/Pengcheng/R/analyse")

pdf("correlation/exp_variables.pdf", width=15, height=10)
# Step 2:
###correlation analysis between explainable variables(median value)====
# names(median_df_ana2)
# 1.1 all
res1 <- rcorr(as.matrix(median_df_ana2[,c(6:34)]),type ="spearman")
res1
diag(res1$P) <- 0 # NA not available changing into 0
# Insignificant correlations are leaved blank
corrplot(res1$r, type="upper", order="original", 
         p.mat = res1$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 0.4,tl.col="black",tl.srt=70,title=
         "Correlation analysis between explainable variables(median value)at global scale",mar=c(0,0,1,0))

# 1.2
# exclude adjusted tree cover, mio_pre_cur as well as mio_tem_cur 
res1_exc_adj<- rcorr(as.matrix(median_df_ana2[,c(6,7,9,10,12,14,15,17,18,
                                                  20,21,23,24,26:29,31,33,34)]),type ="spearman")
res1_exc_adj
diag(res1_exc_adj$P) <- 0 # NA not available changing into 0
# Insignificant correlations are leaved blank
corrplot(res1_exc_adj$r, type="upper", order="original", 
         p.mat = res1_exc_adj$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 0.4,tl.col="black",tl.srt=70,title=
           "Correlation analysis between explainable variables(median value)at global scale",mar=c(0,0,1,0))

# 1.3
# exclude tree cover,human modification index, mio_pre_cur as well as mio_tem_cur 
res1_exc_tc_hm <- rcorr(as.matrix(median_df_ana2[,c(8,11,13,16,19,22,25,27:29,31,33,34)]),type ="spearman")
res1_exc_tc_hm
diag(res1_exc_tc_hm$P) <- 0 # NA not available changing into 0
# Insignificant correlations are leaved blank
corrplot(res1_exc_tc_hm$r, type="upper", order="original", 
         p.mat = res1_exc_tc_hm$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 0.4,tl.col="black",tl.srt=70,title=
           "Correlation analysis between explainable variables(median value)at global scale",mar=c(0,0,1,0))

# get names of realm
realm1 <- unique(median_df_ana2$realm1)

# 2.1 all
#for each realm get correlation relation
foreach(i=realm1) %dopar% {
  res2 <- rcorr(as.matrix(median_df_ana2[median_df_ana2$realm1==i,c(6:34)]),type ="spearman")
  res2
  diag(res2$P) <- 0 # NA not available changing into 0
  # Insignificant correlations are leaved blank
  corrplot(res2$r, type="upper", order="original", 
           p.mat = res2$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
           number.cex = 0.4,tl.col="black",tl.srt=70,title=
             paste0("Correlation analysis between explainable variables(median value) in ",i),mar=c(0,0,1,0))
}

# 2.2 
# exclude adjusted tree cover, mio_pre_cur as well as mio_tem_cur at realm scale
foreach(i=realm1) %dopar% {
  res2_exc_adj <- rcorr(as.matrix(median_df_ana2[median_df_ana2$realm1==i,c(6,7,9,10,12,14,15,17,18,
                                                                           20,21,23,24,26:29,31,33,34)]),type ="spearman")
  res2_exc_adj
  diag(res2_exc_adj$P) <- 0 # NA not available changing into 0
  # Insignificant correlations are leaved blank
  corrplot(res2_exc_adj$r, type="upper", order="original", 
           p.mat = res2_exc_adj$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
           number.cex = 0.4,tl.col="black",tl.srt=70,title=
             paste0("Correlation analysis between explainable variables(median value) in ",i),mar=c(0,0,1,0))
} 
# 2.3
# exclude tree cover,human modification index, mio_pre_cur as well as mio_tem_cur 
foreach(i=realm1) %dopar% {
  res2_exc_tc_hm <- rcorr(as.matrix(median_df_ana2[median_df_ana2$realm1==i,c(8,11,13,16,19,22,25,27:29,31,33,34)]),type ="spearman")
  res2_exc_tc_hm
  diag(res2_exc_tc_hm$P) <- 0 # NA not available changing into 0
  # Insignificant correlations are leaved blank
  corrplot(res2_exc_tc_hm$r, type="upper", order="original", 
           p.mat = res2_exc_tc_hm$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
           number.cex = 0.4,tl.col="black",tl.srt=70,title=
             paste0("Correlation analysis between explainable variables(median value) in ",i),mar=c(0,0,1,0))
}


# ???:
# Step 2:
###correlation analysis between explainable variables(mean value)====
# names(mean_df_ana2)
# 1.1 all
res3 <- rcorr(as.matrix(mean_df_ana2[,c(6:34)]),type ="spearman")
res3
diag(res3$P) <- 0 # NA not available changing into 0
# Insignificant correlations are leaved blank
corrplot(res3$r, type="upper", order="original", 
         p.mat = res3$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 0.4,tl.col="black",tl.srt=70,title=
           "Correlation analysis between explainable variables(mean value)at global scale",mar=c(0,0,1,0))

# 1.2
# exclude adjusted tree cover, mio_pre_cur as well as mio_tem_cur 
res3_exc_adj<- rcorr(as.matrix(mean_df_ana2[,c(6,7,9,10,12,14,15,17,18,
                                                 20,21,23,24,26:29,31,33,34)]),type ="spearman")
res3_exc_adj
diag(res3_exc_adj$P) <- 0 # NA not available changing into 0
# Insignificant correlations are leaved blank
corrplot(res3_exc_adj$r, type="upper", order="original", 
         p.mat = res3_exc_adj$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 0.4,tl.col="black",tl.srt=70,title=
           "Correlation analysis between explainable variables(mean value)at global scale",mar=c(0,0,1,0))

# 1.3
# exclude tree cover,human modification index, mio_pre_cur as well as mio_tem_cur 
res3_exc_tc_hm <- rcorr(as.matrix(mean_df_ana2[,c(8,11,13,16,19,22,25,27:29,31,33,34)]),type ="spearman")
res3_exc_tc_hm
diag(res3_exc_tc_hm$P) <- 0 # NA not available changing into 0
# Insignificant correlations are leaved blank
corrplot(res3_exc_tc_hm$r, type="upper", order="original", 
         p.mat = res3_exc_tc_hm$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 0.4,tl.col="black",tl.srt=70,title=
           "Correlation analysis between explainable variables(mean value)at global scale",mar=c(0,0,1,0))

# 2.1 all
#for each realm get correlation relation
foreach(i=realm1) %dopar% {
  res4 <- rcorr(as.matrix(mean_df_ana2[mean_df_ana2$realm1==i,c(6:34)]),type ="spearman")
  res4
  diag(res4$P) <- 0 # NA not available changing into 0
  # Insignificant correlations are leaved blank
  corrplot(res4$r, type="upper", order="original", 
           p.mat = res4$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
           number.cex = 0.4,tl.col="black",tl.srt=70,title=
             paste0("Correlation analysis between explainable variables(mean value) in ",i),mar=c(0,0,1,0))
}

# 2.2
# exclude adjusted tree cover, mio_pre_cur as well as mio_tem_cur at realm scale
foreach(i=realm1) %dopar% {
  res4_exc_adj <- rcorr(as.matrix(mean_df_ana2[mean_df_ana2$realm1==i,c(6,7,9,10,12,14,15,17,18,                                                                           20,21,23,24,26:29,31,33,34)]),type ="spearman")
  res4_exc_adj
  diag(res4_exc_adj$P) <- 0 # NA not available changing into 0
  # Insignificant correlations are leaved blank
  corrplot(res4_exc_adj$r, type="upper", order="original", 
           p.mat = res4_exc_adj$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
           number.cex = 0.4,tl.col="black",tl.srt=70,title=
             paste0("Correlation analysis between explainable variables(mean value) in ",i),mar=c(0,0,1,0))
} 
# 2.3
# exclude tree cover,human modification index, mio_pre_cur as well as mio_tem_cur 
foreach(i=realm1) %dopar% {
  res2_exc_tc_hm <- rcorr(as.matrix(mean_df_ana2[mean_df_ana2$realm1==i,c(8,11,13,16,19,22,25,27:29,31,33,34)]),type ="spearman")
  res2_exc_tc_hm
  diag(res2_exc_tc_hm$P) <- 0 # NA not available changing into 0
  # Insignificant correlations are leaved blank
  corrplot(res2_exc_tc_hm$r, type="upper", order="original", 
           p.mat = res2_exc_tc_hm$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
           number.cex = 0.4,tl.col="black",tl.srt=70,title=
             paste0("Correlation analysis between explainable variables(mean value) in ",i),mar=c(0,0,1,0))
}


# Step 3: Run dev.off() to create the file!
dev.off()




# step1:save a pdf document
pdf("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/climate_patterns.pdf", width=15, height=10)
# patterns of RU distributed under different variables gradients(climate variables)====
# names(mean_df_ana2)
var <- colnames(mean_df_ana2[,c(27:34)])
# names(var)

# only for median value of explainable variables
# sta_RU_onlyin
#  step2:plot
# i="mio_tem_cur"
foreach(i=var,.packages=c('ggplot2'))%do% {
  # global scale
  plot1 <- 
    ggplot(mean_df_ana2)+
    geom_point(
      aes(mean_df_ana2[,i], log(RU_onlyIn)),
      alpha = 0.4,size=2
    ) +
    labs(title=paste0("Species Range Unfilling(onlyin,",i,")"),x=i,y="Range Unfilling(onlyin)") +
    theme( 
      plot.title = element_text(hjust = 0.5,size=16))+
    geom_smooth( 
      aes(x=mean_df_ana2[,i],y=log(RU_onlyIn)),
      se = F,col='red', size=0.5,method=glm)+
    # ylim(c(0, quantile(mean_df_ana2$RU_onlyIn, 0.95)))+
    xlim(quantile(mean_df_ana2[,i], 0.05),quantile(mean_df_ana2[,i], 0.95))
  
  
  
  
  
  
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_onlyin.png", sep = "")
         ,plot= plot1,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot1) 
  
  # realm scale
  plot2 <- 
    ggplot(mean_df_ana2)+
    geom_point(
      aes(mean_df_ana2[,i], log(RU_onlyIn)),
      alpha = 0.4,size=2
    ) +
    labs(title=paste0("Species Range Unfilling(onlyin,",i,")"),x=i,y="Range Unfilling(onlyin)") +
    theme( 
      plot.title = element_text(hjust = 0.5,size=16))+
    geom_smooth( 
      aes(x=mean_df_ana2[,i],y=log(RU_onlyIn)),
      se = F,col='red', size=0.5,method=glm)+
    # ylim(c(0, quantile(mean_df_ana2$RU_onlyIn, 0.95)))+
    xlim(quantile(mean_df_ana2[,i], 0.05),quantile(mean_df_ana2[,i], 0.95)) +
    facet_wrap(vars(realm1))
  
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_onlyin_facet.png", sep = "")
         ,plot= plot2,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot2) 
  
  # # sta_RU_inout 
  # # global scale
  # plot3 <- 
  #   ggplot(mean_df_ana2)+
  #   geom_point(
  #     aes(mean_df_ana2[,i],RU_inout),
  #     alpha = 0.4,size=2
  #   ) +
  #   labs(title=paste0("Species Range Unfilling(inout,",i,")"),x=i,y="Range Unfilling(inout)") +
  #   theme( 
  #     plot.title = element_text(hjust = 0.5,size=16))+
  #   geom_smooth( 
  #     aes(x=mean_df_ana2[,i],y=RU_onlyIn),
  #     se = F,col='red', size=0.5,method=glm)+
  #   ylim(c(0, quantile(mean_df_ana2$RU_inout, 0.95)))+xlim(quantile(mean_df_ana2[,i], 0.05),
  #                                                            quantile(mean_df_ana2[,i], 0.95))
  # 
  # 
  # 
  # ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_inout.png", sep = "")
  #        ,plot= plot3,
  #        height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  # print(plot3)
  # 
  # # realm scale
  # plot4 <- 
  #   ggplot(mean_df_ana2)+
  #   geom_point(
  #     aes(mean_df_ana2[,i], RU_onlyIn),
  #     alpha = 0.4,size=2
  #   ) +
  #   labs(title=paste0("Species Range Unfilling(inout,",i,")"),x=i,y="Range Unfilling(inout)") +
  #   theme( 
  #     plot.title = element_text(hjust = 0.5,size=16))+
  #   geom_smooth( 
  #     aes(x=mean_df_ana2[,i],y=RU_onlyIn),
  #     se = F,col='red', size=0.5,method=glm)+
  #   ylim(c(0, quantile(mean_df_ana2$RU_inout, 0.95)))+xlim(quantile(mean_df_ana2[,i], 0.05),
  #                                                            quantile(mean_df_ana2[,i], 0.95)) +
  #   facet_wrap(vars(realm1))
  # 
  # ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_inout_facet.png", sep = "")
  #        ,plot= plot4,
  #        height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  # print(plot4) 
  # 
}
# step3:end of process
dev.off()


# step1:save a pdf document
pdf("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/land-use_patterns.pdf", width=15, height=10)
# patterns of RU distributed under different variables gradients(land-use variables)====

# names(mean_df_ana2)
var <- colnames(mean_df_ana2[,c(1,2,6:26)])
# names(var)

# only for median value of explainary variables
# sta_RU_onlyin
#  step2:plot
# i="mio_tem_cur"
foreach(i=var,.packages=c('ggplot2'))%do% {
  # global scale
  plot1 <- 
    ggplot(mean_df_ana2)+
    geom_point(
      aes(mean_df_ana2[,i], log(RU_onlyIn)),
      alpha = 0.4,size=2
    ) +
    labs(title=paste0("Species Range Unfilling(onlyin,",i,")"),x=i,y="Range Unfilling(onlyin)") +
    theme( 
      plot.title = element_text(hjust = 0.5,size=16))+
    geom_smooth( 
      aes(x=mean_df_ana2[,i],y=log(RU_onlyIn)),
      se = F,col='red', size=0.5,method=glm)
  # +ylim(c(0, quantile(mean_df_ana2$RU_onlyIn, 0.95)))
  
  
  
  
  
  
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_onlyin.png", sep = "")
         ,plot= plot1,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot1) 
  
  # realm scale
  plot2 <- 
    ggplot(mean_df_ana2)+
    geom_point(
      aes(mean_df_ana2[,i], log(RU_onlyIn)),
      alpha = 0.4,size=2
    ) +
    labs(title=paste0("Species Range Unfilling(onlyin,",i,")"),x=i,y="Range Unfilling(onlyin)") +
    theme( 
      plot.title = element_text(hjust = 0.5,size=16))+
    geom_smooth( 
      aes(x=mean_df_ana2[,i],y=log(RU_onlyIn)),
      se = F,col='red', size=0.5,method=glm)+
    # ylim(c(0, quantile(mean_df_ana2$RU_onlyIn, 0.95))) +
    facet_wrap(vars(realm1))
  
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_onlyin_facet.png", sep = "")
         ,plot= plot2,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  print(plot2) 
  
  # # sta_RU_inout 
  # # global scale
  # plot3 <- 
  #   ggplot(mean_df_ana2)+
  #   geom_point(
  #     aes(mean_df_ana2[,i],RU_inout),
  #     alpha = 0.4,size=2
  #   ) +
  #   labs(title=paste0("Species Range Unfilling(inout,",i,")"),x=i,y="Range Unfilling(inout)") +
  #   theme( 
  #     plot.title = element_text(hjust = 0.5,size=16))+
  #   geom_smooth( 
  #     aes(x=mean_df_ana2[,i],y=RU_inout),
  #     se = F,col='red', size=0.5,method=glm)+
  #   ylim(c(0, quantile(mean_df_ana2$RU_inout, 0.95)))
  # 
  # 
  # 
  # ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_inout.png", sep = "")
  #        ,plot= plot3,
  #        height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  # print(plot3)
  # 
  # # realm scale
  # plot4 <- 
  #   ggplot(mean_df_ana2)+
  #   geom_point(
  #     aes(mean_df_ana2[,i], RU_inout),
  #     alpha = 0.4,size=2
  #   ) +
  #   labs(title=paste0("Species Range Unfilling(inout,",i,")"),x=i,y="Range Unfilling(inout)") +
  #   theme( 
  #     plot.title = element_text(hjust = 0.5,size=16))+
  #   geom_smooth( 
  #     aes(x=mean_df_ana2[,i],y=RU_inout),
  #     se = F,col='red', size=0.5,method=glm)+
  #   ylim(c(0, quantile(mean_df_ana2$RU_inout, 0.95))) +
  #   facet_wrap(vars(realm1))
  # 
  # ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_inout_facet.png", sep = "")
  #        ,plot= plot4,
  #        height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  # print(plot4) 
  # 
}
# step3:end of process
dev.off()

# step1:save a pdf document
pdf("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/adjustedtree_patterns.pdf", width=15, height=10)
# patterns of RU distributed under different variables gradients(adjusted tree cover(change) only)====

# names(mean_df_ana2)
var <- colnames(mean_df_ana2[,c(8,11,13,16,19,22,25)])
# names(var)

# only for median value of explainary variables
# sta_RU_onlyin
#  step2:plot
# i="mio_tem_cur"
foreach(i=var,.packages=c('ggplot2'))%do% {
  # global scale
  plot1 <- 
    ggplot(mean_df_ana2)+
    geom_point(
      aes(mean_df_ana2[,i], log(RU_onlyIn)),
      alpha = 0.4,size=2
    ) +
    labs(title=paste0("Species Range Unfilling(onlyin,",i,")"),x=i,y="Range Unfilling(onlyin)") +
    theme( 
      plot.title = element_text(hjust = 0.5,size=16))+
    geom_smooth( 
      aes(x=mean_df_ana2[,i],y=log(RU_onlyIn)),
      se = F,col='red', size=0.5,method=glm)
  # +ylim(c(0, quantile(mean_df_ana2$RU_onlyIn, 0.95)))
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_onlyin.png", sep = "")
         ,plot= plot1,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  
  print(plot1) 
  
  # realm scale
  plot2 <- 
    ggplot(mean_df_ana2)+
    geom_point(
      aes(mean_df_ana2[,i], log(RU_onlyIn)),
      alpha = 0.4,size=2
    ) +
    labs(title=paste0("Species Range Unfilling(onlyin,",i,")"),x=i,y="Range Unfilling(onlyin)") +
    theme( 
      plot.title = element_text(hjust = 0.5,size=16))+
    geom_smooth( 
      aes(x=mean_df_ana2[,i],y=log(RU_onlyIn)),
      se = F,col='red', size=0.5,method=glm)+
    # ylim(c(0, quantile(mean_df_ana2$RU_onlyIn, 0.95))) +
    facet_wrap(vars(realm1))
  ggsave(filename = paste0("D:/quercus/Partage/Pengcheng/R/analyse/distribution_patterns/tif/", i,"_onlyin_facet.png", sep = "")
         ,plot= plot2,
         height = 10, width = 15, dpi = 320)   # The directory you want to save the file in
  
  
  print(plot2) 
  
  # sta_RU_inout 
  # global scale
  # plot3 <- 
  #   ggplot(mean_df_ana2)+
  #   geom_point(
  #     aes(mean_df_ana2[,i],RU_inout),
  #     alpha = 0.4,size=2
  #   ) +
  #   labs(title=paste0("Species Range Unfilling(inout,",i,")"),x=i,y="Range Unfilling(inout)") +
  #   theme( 
  #     plot.title = element_text(hjust = 0.5,size=16))+
  #   geom_smooth( 
  #     aes(x=mean_df_ana2[,i],y=RU_inout),
  #     se = F,col='red', size=0.5,method=glm)+
  #   ylim(c(0, quantile(mean_df_ana2$RU_inout, 0.95)))
  # 
  # 
  # 
  # 
  # print(plot3)
  # 
  # # realm scale
  # plot4 <- 
  #   ggplot(mean_df_ana2)+
  #   geom_point(
  #     aes(mean_df_ana2[,i], RU_inout),
  #     alpha = 0.4,size=2
  #   ) +
  #   labs(title=paste0("Species Range Unfilling(inout,",i,")"),x=i,y="Range Unfilling(inout)") +
  #   theme( 
  #     plot.title = element_text(hjust = 0.5,size=16))+
  #   geom_smooth( 
  #     aes(x=mean_df_ana2[,i],y=RU_inout),
  #     se = F,col='red', size=0.5,method=glm)+
  #   ylim(c(0, quantile(mean_df_ana2$RU_inout, 0.95))) +
  #   facet_wrap(vars(realm1))
  # 
  # 
  # print(plot4) 
  # 
}
# step3:end of process
dev.off()




#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code")
# mean values of explanatory variables
mean_df_ana2 <- readRDS("mean_df_ana2.rds")
head(mean_df_ana2)
# downscale variables precipitation variables to more or less same scale when fitting a model
mean_df_ana2$cur_pre_median <- mean_df_ana2$cur_pre_median /1000
mean_df_ana2$lgm_pre_cur <- mean_df_ana2$lgm_pre_cur /100
mean_df_ana2$mio_pre_lgm <- mean_df_ana2$mio_pre_lgm /100
# mean_df_ana2$Atree_median2013AD <- mean_df_ana2$Atree_median2013AD /10

#change the column name
colnames(mean_df_ana2)[c(4,11,36)] <- c("Range_Unfilling","atr_median","realm")

#mixed effect model====
#comparasion between glm and glmm
model = glm(Range_Unfilling~
              atr_median + dif_atr_2013_1950+
              dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC
            +mio_pre_lgm +mio_tem_lgm+lgm_pre_cur
            +lgm_tem_cur  +cur_pre_median +cur_tem_median  , data = mean_df_ana2,family= gaussian(link='log'))

model1 = glmer(Range_Unfilling~ 
                atr_median + dif_atr_2013_1950+ 
                dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
             +mio_pre_lgm +mio_tem_lgm+lgm_pre_cur
              +lgm_tem_cur  +cur_pre_median +cur_tem_median + (1 | realm) , data = mean_df_ana2
             ,family= gaussian(link='log'))
anova(model1,model) # select model1(glmm) as AIC and BIC are smaller
tab_model(model,model1, show.df = TRUE)
vif(model1)


# Remove mio_pre_lgm based on vif
# glm
model_glm = glm(Range_Unfilling~ 
               atr_median + dif_atr_2013_1950+ 
               dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
               +mio_tem_lgm
             +lgm_tem_cur  +cur_pre_median +cur_tem_median , data = mean_df_ana2
             ,family= gaussian(link='log'))
vif(model_glm)
# glmm model with random effect
model_effect = glmer(Range_Unfilling~ 
                       atr_median + dif_atr_2013_1950+ 
                       dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                     +mio_tem_lgm +lgm_pre_cur
                     +lgm_tem_cur  +cur_pre_median +cur_tem_median+(1 | realm) , data = mean_df_ana2
                     ,family= gaussian(link='log'))
vif(model_effect)
summary(model_effect)

cooksd <- cooks.distance(model_effect)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(mean_df_ana2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red"
     ,pos=3)  # add labels 
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
mean_df_ana2 <- mean_df_ana2[!(row.names(mean_df_ana2)) %in% influential, ]
model_effect = glmer(Range_Unfilling~ 
                       atr_median + dif_atr_2013_1950+ 
                       dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                     +mio_tem_lgm +lgm_pre_cur
                     +lgm_tem_cur  +cur_pre_median +cur_tem_median+(1 | realm) , data = mean_df_ana2
                     ,family= gaussian(link='log'))

summary(model_effect)
write.csv(x=mean_df_ana2,paste0("D:/quercus/Partage/Pengcheng/R/analyse/code","/final_analysis.csv"))
# plot(model_effect)
#variance inflation factor (or VIF)
vif(model_effect)
a<- tab_model(model_effect, collapse.ci = TRUE,
            pred.labels = c("Intercept", "ATC", "EPSIR", "EPFIR",
                            "ED", "AE", 
                            "EAE","MTA","LGMPA","LGMTA","MAP","MAT"),
            dv.labels = "Rang unfilling",
            string.pred = "Coeffcient",
            string.p = "P Value")


variables=c("atr_median" , "dif_atr_2013_1950",
  "dif_atr_1950_1850",  "dif_atr_1850_1500" ,"dif_atr_1500_0" ,"dif_atr_0_10kBC" 
 ,"mio_tem_lgm","lgm_pre_cur"
,"lgm_tem_cur"  ,"cur_pre_median" ,"cur_tem_median")

# glmm model with random effect and random slope
foreach(i=variables,.packages = as.character(packs)) %dopar% {
model_slope = glmer(Range_Unfilling~ 
                      atr_median + dif_atr_2013_1950+ 
                      dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                    +mio_tem_lgm +lgm_pre_cur
                    +lgm_tem_cur  +cur_pre_median +cur_tem_median + (1+ get(i)| realm) , data = mean_df_ana2
               ,family= gaussian(link='log'))  # dif_atr_1500_0 cannot compute

a <- AIC(model_glm,model_effect,model_slope)
b <- BIC(model_glm,model_effect,model_slope)

#df to store results of AIC and BIC
df <- data.frame(a,b)
df$variable <- i
df$df.1 <- NULL
vif(model_slope)
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/GLMM_results/')
dir.create(outputSpDir, recursive = T)
write.csv(x=df,paste0(outputSpDir,i,".csv"))
saveRDS(df,paste0(outputSpDir,i,".rds"))
}

# read model performance results
rds <- list.files(path="D:/quercus/Partage/Pengcheng/R/analyse/GLMM_results/",
                  pattern=".rds$",
                  full.names = TRUE)
performance_model = lapply(rds, readRDS)
performance_model = dplyr::bind_rows(performance_model)
# it turns out random slope may not necessary

# in case plotting GLMM with random slope
# see realm coefficients
# model_coefs <- coef(model_slope)$realm %>% 
#   rename(Intercept = `(Intercept)`, Slope = i) %>% 
#   rownames_to_column("realm")
# 
# # see coefficients
# model_coefs
# # new df with coefficients and random slope
# glmm_realm_atr_median <- left_join(mean_df_ana2, model_coefs, by = "realm")
# # plot realm with different coefficients and random slope
# model_coef_plot <- ggplot(data = glmm_realm_atr_median, 
#                           mapping = aes(x = get(i), 
#                                         y = log(Range_Unfilling), 
#                                         colour = realm)
# ) +
#   geom_point(na.rm = T, alpha = 0.5) +
#   geom_abline(aes(intercept = Intercept, 
#                   slope = Slope,
#                   colour = realm
#   ),
#   size = 1.5
#   ) +
#   scale_y_continuous() +
#   scale_x_continuous() +
#   theme(legend.position = "top")
# 
# model_coef_plot %+% glmm_realm_atr_2013_1950 

# due to the fact it cannot be computed just using the mean_df_ana2 dataset, so here I use dataset
# (refers to final_analysis.csv) which derived from previous mixed effect model using adjusted tree 
# cover that incorporating cook distance to exclude some extreme values
# glm
model = glm(Range_Unfilling ~  tr_median + hm_median 
               + dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
               +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
               +cur_pre_median +cur_tem_median +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
               +lgm_tem_cur 
                , data =  mean_df_ana2,family= gaussian(link='log'))
vif(model)

### final glmm====
model3 = glmer(Range_Unfilling ~  tr_median + hm_median 
              + dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
              +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
              +cur_pre_median +cur_tem_median +lgm_pre_cur+lgm_tem_cur +mio_pre_lgm  +mio_tem_lgm 
              
              + (1 | realm) , data =  mean_df_ana2,family= gaussian(link='log'))
vif(model3)
#  R2 is smaller
# cooksd <- cooks.distance(model3)
# # Plot the Cook's Distance using the traditional 4/n criterion
# sample_size <- nrow(mean_df_ana2)
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4/sample_size, col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red"
#      ,pos=3)  # add labels 
# # Removing Outliers
# # influential row numbers
# influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
# mean_df_ana2 <- mean_df_ana2[!(row.names(mean_df_ana2)) %in% influential, ]
# model4 = glmer(Range_Unfilling ~  tr_median + hm_median 
#                + dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
#                +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
#                +cur_pre_median +cur_tem_median +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
#                +lgm_tem_cur 
#                + (1 | realm) , data =  mean_df_ana2,family= gaussian(link='log'))
# 
# summary(model4)

anova(model3,model)
tab_model(model3, collapse.ci = TRUE,
pred.labels = c("Intercept", "TC","HM","TC-EPSIR","HM-EPSIR", "TC-EPFIR","HM-EPFIR",
                "TC-ED","HM-ED","TC-AE","HM-AE",
                "TC-EAE","HM-EAE","MAP","MAT","LGMPA","LGMTA","MPA","MTA"),
dv.labels = "Rang unfilling",
string.p = "P Value",
file="D:/quercus/Partage/Pengcheng/R/analyse/GLMM_results/final.html")
#variance inflation factor (or VIF)
a <- data.frame(vif(model3) )
write_csv (a,file = "GLMM_vif.csv")
write_rds (a,file = "GLMM_vif.rds")
vif(model4)  
#create vector of VIF values
vif_values <- vif(model4)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)



# how many species in each realm
sort(table(mean_df_ana2$realm),decreasing=T)
# fit glm====
# followd order 
# for adjusted tree cover
# 1.Neotropic ====
Neotropic<- mean_df_ana2[mean_df_ana2$realm=="Neotropic",]
# hist(Neotropic$Range_Unfilling)
model_Neotropic <- glm(Range_Unfilling~ atr_median +
                         dif_atr_2013_1950+ 
                         dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                        +mio_tem_lgm +lgm_pre_cur
                       +lgm_tem_cur +cur_pre_median +cur_tem_median , data = Neotropic
                       ,family= gaussian(link='log'))

summary(model_Neotropic)
# plot(model_Neotropic)
cooksd <- cooks.distance(model_Neotropic)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Neotropic)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Neotropic <- Neotropic[!(row.names(Neotropic) %in% influential), ]
model_Neotropic.a <- glm(Range_Unfilling~ 
                           atr_median +
                           dif_atr_2013_1950+ 
                           dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                         +mio_tem_lgm +lgm_pre_cur
                         +lgm_tem_cur +cur_pre_median +cur_tem_median, data = Neotropic
                         ,family= gaussian(link='log'))

summary(model_Neotropic.a)
# plot(model_Neotropic.a)
#variance inflation factor (or VIF)
vif(model_Neotropic.a) # remove mio_pre_lgm 
#create vector of VIF values
vif_values <- vif(model_Neotropic.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

p <- ggplot(aes(x=Range_Unfilling), data=Neotropic)
p + geom_density()+xlim(0,30)

# for tree cover and human modification
Neotropic<- mean_df_ana2[mean_df_ana2$realm=="Neotropic",]
model_Neotropic <- glm(Range_Unfilling~ 
                         dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
                       +dif_tr_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
                       +mio_tem_lgm +lgm_pre_cur
                       +lgm_tem_cur+cur_pre_median +cur_tem_median, data = Neotropic
                       ,family= gaussian(link='log')) 
summary(model_Neotropic)
# plot(model_Neotropic)
cooksd <- cooks.distance(model_Neotropic)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Neotropic)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Neotropic <- Neotropic[!(row.names(Neotropic) %in% influential), ]
model_Neotropic.b <- glm(Range_Unfilling~ 
                         dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850
                         + dif_tr_1850_1500  +dif_hm_1850_1500
                       +dif_tr_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
                       +mio_tem_lgm +lgm_pre_cur
                       +lgm_tem_cur+cur_pre_median +cur_tem_median , data = Neotropic
                       ,family= gaussian(link='log'))  
summary(model_Neotropic.b)
with(summary(model_Neotropic.b), 1 - deviance/null.deviance)
#variance inflation factor (or VIF)
a <- data.frame(vif(model_Neotropic.b)) # remove tr_median,hm_median,mio_pre_lgm,dif_hm_1500_0 
#create vector of VIF values
vif_values <- vif(model_Neotropic.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
write_csv (a,
           file = "Neotropic_vif.csv")
write_rds (a,file = "Neotropic_vif.rds")

# for adjusted tree cover
# 2.Afrotropic ====
Afrotropic<- mean_df_ana2[mean_df_ana2$realm=="Afrotropic",]
model_Afrotropic <- glm(Range_Unfilling~ 
                          atr_median + dif_atr_2013_1950+ 
                          dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                        +mio_tem_lgm  + lgm_tem_cur + cur_tem_median + cur_pre_median 
                       , data = Afrotropic
                       ,family= gaussian(link='log'))


# plot(model_Afrotropic)
cooksd <- cooks.distance(model_Afrotropic)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Afrotropic)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels


# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Afrotropic <- Afrotropic[!(row.names(Afrotropic) %in% influential), ]
model_Afrotropic.a <- glm(Range_Unfilling~ 
                         atr_median + dif_atr_2013_1950+ 
                         dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                       +mio_tem_lgm  + lgm_tem_cur + cur_tem_median + cur_pre_median
                       , data = Afrotropic
                       ,family= gaussian(link='log'))

summary(model_Afrotropic.a)
#variance inflation factor (or VIF)
vif(model_Afrotropic.a) # remove mio_pre_lgm,lgm_pre_cur 
#create vector of VIF values
vif_values <- vif(model_Afrotropic.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
# plot(model_Afrotropic)
p <- ggplot(aes(x=Range_Unfilling), data=Afrotropic)
p + geom_density()+xlim(0,30)

# for tree cover and human modification
Afrotropic<- mean_df_ana2[mean_df_ana2$realm=="Afrotropic",]
model_Afrotropic <- glm(Range_Unfilling~ 
                          tr_median + hm_median 
                        + dif_tr_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
                        +dif_tr_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
                        +mio_tem_lgm 
                        +lgm_tem_cur+cur_tem_median, data = Afrotropic
                        ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Afrotropic)
# plot(model_Afrotropic)
cooksd <- cooks.distance(model_Afrotropic)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Afrotropic)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Afrotropic <- Afrotropic[!(row.names(Afrotropic) %in% influential), ]
model_Afrotropic.b <- glm(Range_Unfilling~ 
                         tr_median + hm_median 
                       + dif_tr_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
                       +dif_tr_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
                         +mio_tem_lgm 
                       +lgm_tem_cur+cur_tem_median, data = Afrotropic,
                       family= gaussian(link='log'))

summary(model_Afrotropic.b)
with(summary(model_Afrotropic.b), 1 - deviance/null.deviance)
#variance inflation factor (or VIF)
a <- data.frame(vif(model_Afrotropic.b))# remove mio_pre_lgm,lgm_pre_cur,cur_pre_median,dif_hm_1500_0 , dif_hm_2013_1950
#create vector of VIF values
vif_values <- vif(model_Afrotropic.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
# plot(model_Afrotropic)
write_csv (a,
           file = "Afrotropic_vif.csv")
write_rds (a,file = "Afrotropic_vif.rds")

# for adjusted tree cover
# 3.Australasia ====
Australasia<- mean_df_ana2[mean_df_ana2$realm=="Australasia",]
model_Australasia <- glm(Range_Unfilling~ 
                           atr_median + dif_atr_2013_1950+ 
                           dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                         +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur 
                         +lgm_tem_cur +cur_tem_median, data = Australasia
                         ,family= gaussian(link='log'))

summary(model_Australasia)
# plot(model_Australasia)
cooksd <- cooks.distance(model_Australasia)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Australasia)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Australasia <- Australasia[!(row.names(Australasia) %in% influential), ]
model_Australasia.a <- glm(Range_Unfilling~ 
                           atr_median + dif_atr_2013_1950+ 
                           dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0 +dif_atr_0_10kBC 
                         +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur 
                         +lgm_tem_cur +cur_tem_median, data = Australasia
                         ,family= gaussian(link='log'))

summary(model_Australasia.a)
# plot(model_Australasia.a)
#variance inflation factor (or VIF)
vif(model_Australasia.a) # remove cur_pre_median
#create vector of VIF values
vif_values <- vif(model_Australasia.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
p <- ggplot(aes(x=Range_Unfilling), data=Australasia)
p + geom_density()+xlim(0,30)


# for tree cover and human modification
Australasia<- mean_df_ana2[mean_df_ana2$realm=="Australasia",]
model_Australasia <- glm(Range_Unfilling~ 
                           dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
                         +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
                         +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
                         +lgm_tem_cur+cur_tem_median, data = Australasia
                         ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Australasia)
# plot(model_Australasia)
cooksd <- cooks.distance(model_Australasia)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Australasia)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Australasia <- Australasia[!(row.names(Australasia) %in% influential), ]
model_Australasia.b <- glm(Range_Unfilling~ 
                           dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850+ dif_hm_1950_1850 + dif_tr_1850_1500  +dif_hm_1850_1500
                         +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  +dif_hm_0_10kBC
                          +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
                         +lgm_tem_cur+cur_tem_median , data = Australasia
                         ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Australasia.b)
#variance inflation factor (or VIF)
a <- data.frame(vif(model_Australasia.b)) # remove cur_pre_median,hm_median,tr_median  
#create vector of VIF values
vif_values <- vif(model_Australasia.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
write_csv (a,
           file = "Australasia_vif.csv")
write_rds (a,file = "Australasia_vif.rds")

# for adjusted tree cover
# 4.Indomalayan ====
Indomalayan<- mean_df_ana2[mean_df_ana2$realm=="Indomalayan",]
model_Indomalayan <- glm(Range_Unfilling~ 
                           atr_median + 
                           dif_atr_2013_1950+  dif_atr_1850_1500 +dif_atr_1500_0 + dif_atr_0_10kBC 
                         +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur 
                         +lgm_tem_cur+cur_tem_median , data = Indomalayan
                         ,family= gaussian(link='log'))

summary(model_Indomalayan)
# plot(model_Indomalayan)
cooksd <- cooks.distance(model_Indomalayan)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Indomalayan)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Indomalayan <- Indomalayan[!(row.names(Indomalayan) %in% influential), ]
model_Indomalayan.a <- glm(Range_Unfilling~ atr_median + 
                       dif_atr_2013_1950+  dif_atr_1850_1500 +dif_atr_1500_0 + dif_atr_0_10kBC 
                           +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
                          +lgm_tem_cur+cur_tem_median , data = Indomalayan
                       ,family= gaussian(link='log'))

summary(model_Indomalayan.a)
# plot(model_Indomalayan)
#variance inflation factor (or VIF)
vif(model_Indomalayan.a) # remove cur_pre_median,dif_atr_1950_1850
#create vector of VIF values
vif_values <- vif(model_Indomalayan.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
p <- ggplot(aes(x=Range_Unfilling), data=Indomalayan)
p + geom_density()+xlim(0,30)

# for tree cover and human modification
Indomalayan<- mean_df_ana2[mean_df_ana2$realm=="Indomalayan",]
model_Indomalayan <- glm(Range_Unfilling~ 
                           dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850  
                         +dif_hm_1850_1500+dif_tr_1500_0+dif_hm_1500_0  
                         +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
                         +lgm_tem_cur+ cur_tem_median, data = Indomalayan
                         ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Indomalayan)
# plot(model_Indomalayan)
cooksd <- cooks.distance(model_Indomalayan)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Indomalayan)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Indomalayan <- Indomalayan[!(row.names(Indomalayan) %in% influential), ]
model_Indomalayan.b <- glm(Range_Unfilling~ 
                             dif_tr_2013_1950+ dif_hm_2013_1950 + dif_tr_1950_1850  
                           +dif_hm_1850_1500+dif_tr_1500_0+dif_hm_1500_0  
                            +mio_pre_lgm  +mio_tem_lgm +lgm_pre_cur
                          +lgm_tem_cur+ cur_tem_median, data = Indomalayan
                          ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Indomalayan.b)
#variance inflation factor (or VIF)
a <- data.frame(vif(model_Indomalayan.b)) # remove dif_hm_0_10kBC,dif_hm_1950_1850,dif_tr_1850_1500,hm_median,tr_median,
#cur_pre_median,dif_tr_0_10kBC
#create vector of VIF values
vif_values <- vif(model_Indomalayan.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
write_csv (a,
           file = "Australasia_vif.csv")
write_rds (a,file = "Australasia_vif.rds")

# 5.Palearctic ====
Palearctic <- mean_df_ana2[mean_df_ana2$realm=="Palearctic",]
model_Palearctic<- glm(Range_Unfilling~ 
                         atr_median + dif_atr_2013_1950+ 
                         dif_atr_1950_1850 +dif_atr_1850_1500+dif_atr_1500_0 +dif_atr_0_10kBC
                       +mio_pre_lgm   +lgm_pre_cur
                       +lgm_tem_cur+cur_tem_median, data = Palearctic 
                       ,family= gaussian(link='log'))

summary(model_Palearctic )
# plot(model_Palearctic )
cooksd <- cooks.distance(model_Palearctic )
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Palearctic )
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Palearctic  <- Palearctic [!(row.names(Palearctic ) %in% influential), ]
model_Palearctic.a <- glm(Range_Unfilling~ 
                           atr_median + dif_atr_2013_1950+ 
                           dif_atr_1950_1850 +dif_atr_1850_1500+dif_atr_1500_0 +dif_atr_0_10kBC
                          +mio_pre_lgm   +lgm_pre_cur
                         +lgm_tem_cur+cur_tem_median, data = Palearctic 
                         ,family= gaussian(link='log'))

summary(model_Palearctic.a)
# plot(model_Palearctic.a)
#variance inflation factor (or VIF)
vif(model_Palearctic.a) # remove cur_pre_median,mio_tem_lgm
#create vector of VIF values
vif_values <- vif(model_Palearctic.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
p <- ggplot(aes(x=Range_Unfilling), data=Palearctic)
p + geom_density()+xlim(0,30)

# for tree cover and human modification
Palearctic <- mean_df_ana2[mean_df_ana2$realm=="Palearctic",]
model_Palearctic  <- glm(Range_Unfilling~ 
                         dif_tr_2013_1950+ dif_tr_1950_1850+ dif_hm_1950_1850  +dif_hm_1850_1500
                       +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  
                       +cur_tem_median +mio_pre_lgm +lgm_pre_cur
                       +lgm_tem_cur, data = Palearctic
                       ,family= gaussian(link='log')
                       )  
summary(model_Palearctic )
# plot(model_Palearctic )
cooksd <- cooks.distance(model_Palearctic )
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Palearctic )
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Palearctic  <- Palearctic [!(row.names(Palearctic ) %in% influential), ]
model_Palearctic.b <- glm(Range_Unfilling~ 
                          dif_tr_2013_1950+ dif_tr_1950_1850+ dif_hm_1950_1850  +dif_hm_1850_1500
                         +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC 
                          +mio_pre_lgm  +lgm_pre_cur
                         +lgm_tem_cur+cur_tem_median, data = Palearctic
                         ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Palearctic.b)
#variance inflation factor (or VIF)
a <- data.frame(vif(model_Palearctic.b)) # remove cur_pre_median,dif_hm_0_10kBC, mio_tem_lgm,hm_median,tr_median,dif_hm_2013_1950,
#dif_tr_1850_1500,dif_atr_1850_1500
#create vector of VIF values
vif_values <- vif(model_Palearctic.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
write_csv (a,file = "Palearctic_vif.csv")
write_rds (a,file = "Palearctic_vif.rds")

# for adjusted tree cover
# 6.Nearctic ====
Nearctic<- mean_df_ana2[mean_df_ana2$realm=="Nearctic",]
model_Nearctic <- glm(Range_Unfilling~ 
                        atr_median + dif_atr_2013_1950+ 
                        dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0   +dif_atr_0_10kBC 
                      +lgm_pre_cur + lgm_tem_cur +cur_pre_median  +cur_tem_median
                      , data = Nearctic
                      ,family= gaussian(link='log'))

summary(model_Nearctic)
# plot(model_Nearctic)
cooksd <- cooks.distance(model_Nearctic)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Nearctic)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Nearctic <- Nearctic[!(row.names(Nearctic) %in% influential), ]
model_Nearctic.a <- glm(Range_Unfilling~ 
                            atr_median + dif_atr_2013_1950+ 
                            dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0   +dif_atr_0_10kBC 
                        +lgm_pre_cur + lgm_tem_cur +cur_pre_median  +cur_tem_median
                          , data = Nearctic
                        ,family= gaussian(link='log'))

summary(model_Nearctic.a)
# plot(model_Nearctic)
#variance inflation factor (or VIF)
vif(model_Nearctic.a) # remove mio_tem_lgm,mio_pre_lgm
#create vector of VIF values
vif_values <- vif(model_Nearctic.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
p <- ggplot(aes(x=Range_Unfilling), data=Nearctic)
p + geom_density()+xlim(0,30)

# for tree cover and human modification
Nearctic<- mean_df_ana2[mean_df_ana2$realm=="Nearctic",]
model_Nearctic <- glm(Range_Unfilling~ 
                        dif_tr_2013_1950+ dif_hm_2013_1950+dif_tr_1950_1850+ dif_hm_1950_1850 
                      + dif_tr_1850_1500  
                      +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  
                       +lgm_pre_cur
                      +lgm_tem_cur+cur_pre_median +cur_tem_median, data = Nearctic
                      ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Nearctic)
# plot(model_Nearctic)
cooksd <- cooks.distance(model_Nearctic)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Nearctic)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Nearctic <- Nearctic[!(row.names(Nearctic) %in% influential), ]
model_Nearctic.b <- glm(Range_Unfilling~ 
                            dif_tr_2013_1950+ dif_hm_2013_1950+dif_tr_1950_1850+ dif_hm_1950_1850 
                          + dif_tr_1850_1500  
                          +dif_tr_1500_0+dif_hm_1500_0 +dif_tr_0_10kBC  
                           +lgm_pre_cur
                          +lgm_tem_cur+cur_pre_median +cur_tem_median, data = Nearctic
                        ,family= gaussian(link='log'))  
summary(model_Nearctic.b)
# plot(model_Nearctic)
#variance inflation factor (or VIF)
a <- data.frame(vif(model_Nearctic.b)) # remove dif_hm_1850_1500,mio_tem_lgm,hm_median,tr_median,dif_hm_0_10kBC,mio_pre_lgm 
#create vector of VIF values
vif_values <- vif(model_Nearctic.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
write_csv (a, file = "Nearctic_vif.csv")
write_rds (a,file = "Nearctic_vif.rds")
# for adjusted tree cover
# 7.Oceania ====
Oceania<- mean_df_ana2[mean_df_ana2$realm=="Oceania",]
model_Oceania <- glm(Range_Unfilling~ 
                       dif_atr_2013_1950+
                       dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0+dif_atr_0_10kBC 
                     +mio_pre_cur+mio_tem_lgm  + lgm_tem_cur 
                     +lgm_pre_cur 
                     , data = Oceania
                     ,family= gaussian(link='log'))

summary(model_Oceania)
# plot(model_Oceania)
cooksd <- cooks.distance(model_Oceania)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Oceania)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Oceania <- Oceania[!(row.names(Oceania) %in% influential), ]
model_Oceania.a <- glm(Range_Unfilling~ dif_atr_2013_1950+
                             dif_atr_1950_1850+  dif_atr_1850_1500 +dif_atr_1500_0+dif_atr_0_10kBC 
                       +mio_pre_cur+mio_tem_lgm  + lgm_tem_cur 
                       +lgm_pre_cur 
                       
                          , data = Oceania
                       ,family= gaussian(link='log'))

summary(model_Oceania.a)
# plot(model_Oceania.a)
#variance inflation factor (or VIF)
vif(model_Oceania.a) # remove cur_tem_median,atr_median,cur_pre_median
#create vector of VIF values
vif_values <- vif(model_Oceania.a)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
p <- ggplot(aes(x=Range_Unfilling), data=Oceania)
p + geom_density()+xlim(0,30)

# for tree cover and human modification
Oceania<- mean_df_ana2[mean_df_ana2$realm=="Oceania",]
model_Oceania <- glm(Range_Unfilling~ 
                       tr_median 
                     + dif_tr_2013_1950+ dif_hm_2013_1950 +dif_hm_1950_1850+ dif_tr_1850_1500  
                     +dif_hm_1500_0 +dif_hm_0_10kBC
                     + mio_pre_lgm  +lgm_tem_cur
                     , data = Oceania
                     ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Oceania)
# plot(model_Oceania)
cooksd <- cooks.distance(model_Oceania)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(Oceania)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) #get row names
Oceania <- Oceania[!(row.names(Oceania) %in% influential), ]
model_Oceania.b <- glm(Range_Unfilling~ 
                             tr_median 
                            +dif_hm_1950_1850+ dif_tr_1850_1500  
                           +dif_hm_1500_0 +dif_hm_0_10kBC
                           + mio_pre_lgm  +lgm_tem_cur
                           , data = Oceania
                       ,family= gaussian(link='log'))  #1850_1500 #cur_tem #mio_tem_lgm
summary(model_Oceania.b)
# plot(model_Oceania.b)
#variance inflation factor (or VIF)
a<-data.frame( vif(model_Oceania.b)) # remove cur_tem_median,cur_pre_median,dif_hm_1850_1500,hm_median,dif_tr_2013_1950
#dif_tr_1500_0, dif_tr_0_10kBC,mio_pre_lgm,dif_tr_1950_1850,dif_hm_2013_1950
#create vector of VIF values
vif_values <- vif(model_Oceania.b)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)
write_csv (a,
           file = "Oceania_vif.csv")
write_rds (a,file = "Oceania_vif.rds")

setwd("D:/quercus/Partage/Pengcheng/R/analyse/GLMM_results")
library(jtools)
library(ggstance)
library(broom.mixed)
# plot GLM results
coef_names <- c("TC"="tr_median" ,"HM"="hm_median",
                "TC-EPSIR"="dif_tr_2013_1950","HM-EPSIR"="dif_hm_2013_1950", 
                "TC-EPFIR"="dif_tr_1950_1850","HM-EPFIR"="dif_hm_1950_1850",
                "TC-ED"="dif_tr_1850_1500","HM-ED"="dif_hm_1850_1500",
                "TC-AE"="dif_tr_1500_0","HM-AE"="dif_hm_1500_0",
                "TC-EAE"="dif_tr_0_10kBC","HM-EAE"="dif_hm_0_10kBC",
                "MAP"="cur_pre_median","MAT"="cur_tem_median",
                "LGMPA"="lgm_pre_cur","LGMTA"="lgm_tem_cur",
                "MPA"="mio_pre_lgm","MTA"="mio_tem_lgm")

# in a plot
plot_summs(model_Neotropic.b,model_Afrotropic.b,model_Australasia.b,
           model_Indomalayan.b,model_Palearctic.b, model_Nearctic.b, model_Oceania.b,
           coefs = coef_names,
           scale = TRUE, robust = TRUE, point.shape = T,
           legend.title = " Realm model",
           model.names = c("Neotropic", "Afrotropic", "Australasia",
                           "Indomalayan","Palearctic","Nearctic","Oceania")) 
#  in a table
a<- export_summs(model_Neotropic.b,model_Afrotropic.b,model_Australasia.b,
             model_Indomalayan.b,model_Palearctic.b, model_Nearctic.b, model_Oceania.b,
             robust = "HC3", coefs = coef_names,
             model.names = c("Neotropic", "Afrotropic", "Australasia",
                      "Indomalayan","Palearctic","Nearctic","Oceania"),
             scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]",
             to.file = "docx", file.name = "test.docx")
library(flextable)
b <- flextable(a) 
flextable::save_as_docx(a,path="test1.docx")
# Stop the cluster
stopCluster(cl)
closeAllConnections()
registerDoSEQ()    ###do the sequential coding


