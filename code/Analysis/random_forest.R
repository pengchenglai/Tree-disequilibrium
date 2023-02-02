rm(list=ls())  ###start from scratch
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code/dataset_RU")
##set up library
packs <- list("tidyverse",'doParallel','foreach','mapview',"sf","Hmisc","corrplot","randomForest"
              ,"pastecs","gtools","terra","lme4","lmerTest","rfPermute","rfUtilities")
lapply(packs, require, character.only = T)


# df with median value of statistics====
median_df_ana2 <- readRDS("median_df_ana2 .rds")
mean_df_ana2 <- readRDS("mean_df_ana2.rds")

# set working directory
setwd("D:/quercus/Partage/Pengcheng/R/analyse")

head(mean_df_ana2)
# downscale variables precipitation variables to more or less same scale when fitting a model
mean_df_ana2$cur_pre_median <- mean_df_ana2$cur_pre_median /1000
mean_df_ana2$lgm_pre_cur <- mean_df_ana2$lgm_pre_cur /100
mean_df_ana2$mio_pre_lgm <- mean_df_ana2$mio_pre_lgm /100
# mean_df_ana2$Atree_median2013AD <- mean_df_ana2$Atree_median2013AD /10
# get names of realm
realm1 <- unique(median_df_ana2$realm1)

# Step 1: Call the pdf command to start the plot
pdf("Random_forest/RF_mean_variabes.pdf", width=15, height=10)

# step2:
# random forest(mean value)
# names(mean_df_ana2)
# select columns of interest
# all variables ====
# choose Rang Unfilling that alpha-hull only overlaps with potential geographical range
meandf1<- mean_df_ana2[,c(4,6:29,31,33,34)]
names(meandf1)
rf1 <- randomForest(data=meandf1,log(RU_onlyIn)~., ntree=1000,importance = TRUE)

# How many trees are needed to reach the minimum error estimate? 
 
which.min(rf1$mse)
rf1

# Using the importance()  function to calculate the importance of each variable
imp1 <- as.data.frame(sort(importance(rf1)[,1],decreasing = TRUE),optional = T)
names(imp1) <- "% Inc MSE"   ###Mean Squared Error##
imp1 <- data.frame(imp1)
colnames(imp1) <- "Variable importance"

varImpPlot(rf1,main= "Relative importance of explanatory variables") # Variable importance plot 

#output 
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/Random_forest/results')
dir.create(outputSpDir, recursive = T)
write.csv(imp1,file=paste0(outputSpDir,'/all_variables.csv'))
saveRDS(imp1,paste0(outputSpDir,'/all_variables.rds'))

# select columns of interest
# adjusted tree cover ====
# choose Rang Unfilling that alpha-hull only overlaps with potential geographical tange
names(mean_df_ana2)
meandf2<- mean_df_ana2[,c(4,11,13,16,19,22,25,27:29,31,33,34)]
names(meandf2)
rf2 <- randomForest(data=meandf2,log(RU_onlyIn)~., ntree=1000,importance = TRUE,)


# How many trees are needed to reach the minimum error estimate? 
 
which.min(rf2$mse)
rf2

# Using the importance()  function to calculate the importance of each variable
imp2 <- as.data.frame(sort(importance(rf2)[,1],decreasing = TRUE),optional = T)
names(imp2) <- "% Inc MSE"   ###Mean Squared Error##
imp2 <- data.frame(imp2)
colnames(imp2) <- "Variable importance"

varImpPlot(rf2,main= "Relative importance of explanatory variables") # Variable importance plot 

#output 
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/Random_forest/results')
dir.create(outputSpDir, recursive = T)
write.csv(imp2,file=paste0(outputSpDir,'/adjusetd_tree.csv'))
saveRDS(imp2,paste0(outputSpDir,'/adjusetd_tree.rds'))

# select columns of interest
# tree and hm====
# choose Rang Unfilling that alpha-hull only overlaps with potential geographical tange
names(mean_df_ana2)
meandf3<- mean_df_ana2[,c(4,6,7,12,14,15,17,18,20,21,23,24,26,27:29,31,33,34)]
names(meandf3)
rf3 <- randomForest(data=meandf3,log(RU_onlyIn)~., ntree=1000,importance = TRUE)

# How many trees are needed to reach the minimum error estimate? 
 
which.min(rf3$mse)
rf3

# Using the importance()  function to calculate the importance of each variable
imp3 <- as.data.frame(sort(importance(rf3)[,1],decreasing = TRUE),optional = T)
names(imp3) <- "% Inc MSE"   ###Mean Squared Error##
imp3 <- data.frame(imp3)
colnames(imp3) <- "Variable importance"

varImpPlot(rf3,main= "Relative importance of explanatory variables") # Variable importance plot


#output 
outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/Random_forest/results')
dir.create(outputSpDir, recursive = T)
write.csv(imp3,file=paste0(outputSpDir,'/tree_hm.csv'))
saveRDS(imp3,paste0(outputSpDir,'/tree_hm.rds'))

# select columns of interest
# choose Rang Unfilling that alpha-hull  overlaps with potential geographical range
# different realm, all variables =====
foreach(i=realm1, .packages=c('randomForest')) %do% {
  meandf4<- mean_df_ana2[ mean_df_ana2$realm1==i,]
  meandf4<- meandf4[,c(4,6:29,31,33,34)]
  names(meandf4)
  
  rf4 <- randomForest(data=meandf4,log(RU_onlyIn)~., ntree=1000,importance = TRUE)
  # How many trees are needed to reach the minimum error estimate? 
   
  which.min(rf4$mse)
  rf4
  
  
  # Using the importance()  function to calculate the importance of each variable
  imp4 <- as.data.frame(sort(importance(rf4)[,1],decreasing = TRUE),optional = T)
  names(imp4) <- "% Inc MSE"   ###Mean Squared Error##
  imp4
  imp4 <- data.frame(imp4)
  colnames(imp4) <- "Variable importance"
  
  varImpPlot(rf4,main=paste0("Relative importance of explanatory variables(",i,")")) # Variable importance plot 
  
  #output 
  outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/Random_forest/results')
  dir.create(outputSpDir, recursive = T)
  write.csv(imp4,file=paste0(outputSpDir,'/all_variables_',i,".csv"))
  saveRDS(imp4,paste0(outputSpDir,'/all_variables_',i,".rds"))
  }

# select columns of interest
# choose Rang Unfilling that alpha-hull  overlaps with potential geographical range
# different realm, adjusted tree cover =====
foreach(i=realm1, .packages=c('randomForest')) %do% {
  meandf5<- mean_df_ana2[ mean_df_ana2$realm1==i,]
  meandf5<- meandf5[,c(4,11,13,16,19,22,25,27:29,31,33,34)]
  names(meandf5)
  
  rf5 <- randomForest(data=meandf5,log(RU_onlyIn)~., ntree=1000,importance = TRUE)
  # How many trees are needed to reach the minimum error estimate? 
   
  which.min(rf5$mse)
  rf5
  
  
  # Using the importance()  function to calculate the importance of each variable
  imp5 <- as.data.frame(sort(importance(rf5)[,1],decreasing = TRUE),optional = T)
  names(imp5) <- "% Inc MSE"   ###Mean Squared Error##
  imp5
  imp5 <- data.frame(imp5)
  colnames(imp5) <- "Variable importance"
  
  varImpPlot(rf5,main=paste0("Relative importance of explanatory variables(",i,")")) # Variable importance plot 
  
  #output 
  outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/Random_forest/results')
  dir.create(outputSpDir, recursive = T)
  write.csv(imp5,file=paste0(outputSpDir,'/adjusted_tree_',i,".csv"))
  saveRDS(imp5,paste0(outputSpDir,'/adjusted_tree_',i,".rds"))
}

# select columns of interest
# choose Rang Unfilling that alpha-hull  overlaps with potential geographical range
# different realm, hm and tree cover =====
foreach(i=realm1, .packages=c('randomForest')) %do% {
  meandf6<- mean_df_ana2[ mean_df_ana2$realm1==i,]
  meandf6<- meandf6[,c(4,6,7,12,14,15,17,18,20,21,23,24,26,27:29,31,33,34)]
  
  names(meandf6)
  
  rf6 <- randomForest(data=meandf6,log(RU_onlyIn)~., ntree=1000,importance = TRUE)
  # How many trees are needed to reach the minimum error estimate? 
   
  which.min(rf6$mse)
  rf6
  
  
  # Using the importance()  function to calculate the importance of each variable
  imp6 <- as.data.frame(sort(importance(rf6)[,1],decreasing = TRUE),optional = T)
  names(imp6) <- "% Inc MSE"   ###Mean Squared Error##
  imp6
  imp6 <- data.frame(imp6)
  colnames(imp6) <- "Variable importance"
  
  varImpPlot(rf6,main=paste0("Relative importance of explanatory variables(",i,")")) # Variable importance plot 
  
  #output 
  outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/analyse/Random_forest/results')
  dir.create(outputSpDir, recursive = T)
  write.csv(imp6,file=paste0(outputSpDir,'/tree_hm_',i,".csv"))
  saveRDS(imp6,paste0(outputSpDir,'/tree_hm_',i,".rds"))
}

# Step 3: Run dev.off() to create the file!
dev.off()

set.seed(123)
rf <- rfPermute(data=meandf3,log(RU_onlyIn)~., ntree=500,num.cores=1,nrep=100,na.action=na.omit)
imp <- importance(rf,sort.by=NULL,decresing=TRUE)
imp %>%
  as_tibble(rownames = "names") %>%
  data.frame() %>%
  mutate(label = if_else(X.IncMSE.pval < 0.001,"***",
                         if_else(X.IncMSE.pval <0.01,"**",
                                 if_else(X.IncMSE.pval<0.05,"*","ns"))),
         X.IncMSE = as.numeric(X.IncMSE)) %>%
  arrange(X.IncMSE) %>%
  mutate(group = if_else(label=="ns","In_sig","Sig"),
         names = forcats::fct_inorder(names)) %>%
  ggplot(aes(x = names, y = X.IncMSE))+
  geom_bar(aes(fill = group),stat = "identity")+
  geom_text(aes(y = X.IncMSE + 1,label = label))+
  labs(x = "", y = "X.IncMSE")+
  coord_flip()
