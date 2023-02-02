#set up working directory and libraries====
rm(list=ls())  ###start from scratch
setwd("D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc")

##set up library
packs <- list("geometry",'doParallel','foreach',"tidyverse",'mapview','raster','terra',"gtools",
              "stringr","data.table","sf","sp")
lapply(packs, require, character.only = T)

# set path for temporary files
terraOptions(memfrac=0.9, tempdir = "D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/tem")
rasterOptions(tmpdir="D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/tem")
# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =20
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

#import species which occurrence data have been cleaned
spOc = list.files(pattern = '.-occNonUnique.rds$',
                  path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/cleanOcc',
                  recursive = T)
spOcc = strsplit(spOc,'/')
spOcc = lapply (spOcc, function (x) x[1])
spOcc = unlist (spOcc)

#select one designated species
##a <- as.data.table(spOc)
###(b <-grep("Acacia_cultriformis",x=a$spOc))

#import species which geographical range(a-hull) have been created
spRan = list.files(pattern = '.shp$',
                   path = 'D:/quercus/Partage/Pengcheng/R/cleaing_data/Rrange',
                   recursive = T)
spRan = strsplit(spRan,'/')
spRan = lapply (spRan, function (x) x[1])
spRan = unlist (spRan)

#import species which potential geographical range(rb) have been created
spPRan1 = list.files(pattern = '.csv$',
                     path = 'D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/1',
                     recursive = T)
spPRan1 = strsplit(spPRan1,'/')
spPRan1 = lapply (spPRan1, function (x) x[1])
spPRan1 = unlist (spPRan1)

#import species which potential geographical range(rb) have been created
spPRan2 = list.files(pattern = '.csv$',
                     path = 'D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/2',
                     recursive = T)
spPRan2 = strsplit(spPRan2,'/')
spPRan2 = lapply (spPRan2, function (x) x[1])
spPRan2 = unlist (spPRan2)

#import species which potential geographical range(rb) have been created
spPRan3 = list.files(pattern = '.csv$',
                     path = 'D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/3',
                     recursive = T)
spPRan3 = strsplit(spPRan3,'/')
spPRan3 = lapply (spPRan3, function (x) x[1])
spPRan3 = unlist (spPRan3)

#import species which potential geographical range(rb) have been created
spPRan4 = list.files(pattern = '.csv$',
                     path = 'D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4',
                     recursive = T)
spPRan4 = strsplit(spPRan4,'/')
spPRan4 = lapply (spPRan4, function (x) x[1])
spPRan4 = unlist (spPRan4)

#all the species that potential geographical range have been created
spPRan <-  c(spPRan1,spPRan2,spPRan3,spPRan4)

#id to do:id number of species with geographical range among cleaned occurrence species data
spDone<- spOcc[(spOcc %in% spRan)]
spDone = foreach(i=spDone) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = spOc)})}
#spDone =  unlist(spDone) #does not work
spDon<-c()
a<-1
foreach(i = spDone) %do% {
  id <- unlist(i) 
  print(id)
  spDon[a]<-id
  a<-a+1
}

#id to do:id number of species with potential geographical range among species 
#with geographical range
sptodo<-  spRan[!spRan %in% spPRan]
sptodo <- spOcc[(spOcc %in% sptodo)]
sptodo = foreach(i=sptodo) %dopar% {
  lapply (i, function (s) {grep (pattern = s,x = spOc)})}
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


#load ecoregions
ecoreg <- st_read("D:/quercus/Partage/Pengcheng/R/cleaing_data/Ecoregion_realms/Ecoregions2017/Ecoregions2017.shp")
#in order to join geometry
eco <- data.table(ecoreg)
eco$ECO_ID.1 <- eco$ECO_ID

# Stop the cluster
stopCluster(cl)
closeAllConnections()
registerDoSEQ()    ###do the sequential coding


#sptod= sample(sptod)

# Set up parallel computing ====
###set up how many cores we will use
##detectCores()   ##to know how many cores we have
mc.cores =14
cl <- makeCluster(mc.cores)
registerDoParallel(cl)

#the loop ==== 
l=foreach(spi = sptod,
          .packages = as.character(packs),
          .errorhandling = 'pass') %dopar% {
            
            #import functions based on supplementary material of Drake (2015) ====
            # RANGE BAG FUNCTIONS
            rangeBag<- function(x, v=100, d=2, p=0.25){
              # Basic version of the range bagging algorithm
              # Args:
              #   x: covariate data at observations
              #   v: number of votes
              #   d: dimension of ranges to bag
              #
              # Returns:
              #   A list of length v containing the individual models
              
              # for testing
              # x; v=100; d=3; p=0.5
              #  x=pres.swd; v=stats$rbV[ii]; d=stats$rbD[ii]; p=stats$rbP[ii]
              
              models=rep(list(),v)
              n <- dim(x)
              # print(n)
              success.counter=1
              fail.counter=0
              while(success.counter<=v){
                vars <- sample.int(n[2], size=d, replace=FALSE)
                #print(vars)
                x0 <- x[,vars]
                
                if(d==1){
                  x1 <- x0[sample(n[1],ceiling(p*n[1]), replace=FALSE)]
                  models[[success.counter]] <- list(vars=vars, endpoints=c(min(x1), max(x1)), data=x1)
                  success.counter=success.counter+1
                }
                else{
                  #if(ceiling(p*n[1])<3) message('Note: your sample proportion (p) resulted in <3 points, so using 3 points' )
                  x1 <- x0[sample(n[1],max(3,ceiling(p*n[1])), replace=FALSE),]
                  idx <- suppressMessages(try(unique(as.vector(geometry::convhulln(x1, options='Pp'))),silent=T))
                  if(!class(idx)=='try-error'){
                    endpoints <- x1[idx,]
                    models[[success.counter]] <- list(vars=vars, endpoints=endpoints, data=unique(x1))
                    success.counter=success.counter+1
                  } else {
                    fail.counter=  fail.counter+1
                  }
                }
              }
              print(paste0(fail.counter,' models failed'))
              return(models)
            }
            
            predictRangeBag <- function(models, x.new){
              # for testing
              # models=rbm; x.new=newEnv
              # Test function for a point to determine the fraction of bags in which the point falls
              # Args:
              #   models: a list of models returned by function rb
              #   x.new: covariate data at new points
              #
              # Returns:
              #   A vector of the same length as x.new containing the fraction of models for each point in which it is included
              v <- length(models)
              d <- ifelse(is.null(dim(models[[1]]$endpoints)), 1, dim(models[[1]]$endpoints)[2])
              n <- dim(x.new)
              out <- numeric(n[1])
              for(i in 1:v){
                #print(i) # counter for troubleshooting
                if(d==1){
                  test.pts <- (models[[i]]$endpoints[1] < x.new[,models[[i]]$vars]) & (x.new[,models[[i]]$vars] < models[[i]]$endpoints[2])
                  out <- out + test.pts
                }else{
                  test.dat <- as.matrix(x.new[,models[[i]]$vars])
                  tri.pts <- geometry::tsearchn(as.matrix(models[[i]]$data), geometry::delaunayn(models[[i]]$data), test.dat)
                  #tri.pts <- tsearchn(as.matrix(models[[i]]$endpoints), delaunayn(models[[i]]$endpoints), test.dat)
                  test.pts <- !is.na(tri.pts$p[,1])
                  out <- out + test.pts
                }
              }
              return(out/v)
            }
            
            projectRangeBag=function(env1,rbm){
              # newEnv=terra::values(env1)
              newEnv=raster::values(env1) 
              not.nas=which(complete.cases(newEnv))
              newEnv=newEnv[not.nas,]
              rbp=predictRangeBag(rbm,newEnv)
              
              preds=!is.na(env1[[1]])
              preds[preds==0]=NA
              preds[!is.na(preds)]=0
              values(preds)[not.nas]=rbp
              preds
            }
            
            occ <-readRDS (spOc [spi])
            occ = unique (occ[,c('decimalLongitude','decimalLatitude')])
            occ <- na.omit(occ)
            spName = basename (spOc [spi]) # get species name
            spName = strsplit (spName,'-occNonUnique.rds')[[1]]
            #class(occ)           
            
            if (nrow(occ)<= 1000) {
              #in order to spatially join
              occ_sf <- st_as_sf(occ,coords=c("decimalLongitude","decimalLatitude"),crs=4326)
              #check where the species is
              #mapview(occ_sf)
              
              #select study area where the points located in and polygons adjacent to these points
              #since st_join keep ecoregion attribute while keeping occ_sf(points) geometry
              #we do join the geometry back via left_join
              #then we extend ecoregion to adjacent ecoregions which share common boarder by st_intersection
              
              sf_use_s2(FALSE)
              studyarea<- st_join(occ_sf,ecoreg,join=st_nearest_feature) ##select polyons that points fall in
              studyarea<-left_join(studyarea,eco[,c("geometry","ECO_ID")],by="ECO_ID") ###get geometry of polygons
              studyarea$geometry.x <- NULL
              studyarea <- studyarea[!duplicated(studyarea$ECO_ID),]  #select unique ecoregion
              studyarea <- studyarea[!studyarea$ECO_ID==0 & !is.na(studyarea$ECO_ID),]
              # # in case join with rock and ice
              studyarea <- st_as_sf(studyarea)  #get back to sf object
              studyarea <- st_intersection(studyarea,ecoreg) #by this way is faster to find adjacent polygons
              studyarea$geometry.y <- NULL
              studyarea<-left_join(studyarea,eco[,c("geometry","ECO_ID.1")],by="ECO_ID.1") ###get geometry of polygons
              studyarea <-  studyarea[!duplicated(studyarea$ECO_ID.1),] #select unique ecoregion
              studyarea <- studyarea[!studyarea$ECO_ID.1==0 & !is.na(studyarea$ECO_ID.1),] 
              # in case intersect with rock and ice
              
              if (sum(studyarea$SHAPE_AREA.1)<= 500) {
                studyarea <- st_as_sf(studyarea)
                studyarea <- terra::vect(studyarea)
                #or do the following but much slower
                # studyarea <- st_join(studyarea,ecoreg)#select ecoregions which shares common boundary
                # colnames(studyarea)[24] <- "ECO_ID" # in order to join
                # studyarea<-left_join(studyarea,eco[,c("geometry","ECO_ID")],by="ECO_ID") ###get geometry of polygons
                # studyarea$geometry.y <- NULL
                # studyarea <-  studyarea[!duplicated(studyarea$ECO_ID),]
                # studyarea <- st_as_sf(studyarea)
                # studyarea <- vect(studyarea)
                #plot(studyarea)
                #print(spName)
                
                
                #use crop since it takes less memory(indeed faster) and mask function does not work
                #nevertheless it keeps the climatic data of study area
                clim = terra::rast(mixedsort(bioVars))
                clim <- subset(clim,c(1,5:14,16:19))
                clim <- terra::crop(clim,studyarea,mask=T,snap="out")
                gc()
                
                #extract variables at occurrence locations
                occClim = terra::extract(clim,occ[,c(1,2)], cells=TRUE)
                occClim <- cbind(occClim,occ)
                occClim<- distinct(occClim) #unique column
                occClim <- na.omit(occClim) #filter rows with NA just in case
                #class(occClim)
                
                #separate occurrences for test and for calibration
                ids = sample (1:nrow (occClim))
                lengthCalibration = round (length(ids)*0.8)
                idsCalibration = ids [1:lengthCalibration]
                idsTest = ids [(lengthCalibration+1) : length(ids)]
                
                occClimCalibrate = occClim [idsCalibration,] #80% for building models
                occClimTest = occClim [idsTest,] #20% for testing models
                
                #View(occClimTest)
                occClimCalibrate <- data.matrix(occClimCalibrate)
                occClimTest <- data.matrix(occClimTest)
                #colnames(occClimCalibrate)
                
                #model fit====  
                rbOcc <- rangeBag(x=occClimCalibrate[,c(2:16)])
                #model goodness of fit
                predsFit = predictRangeBag (models = rbOcc,x.new = occClimTest[,c(2:16)])
                #this are the predictions, you will need to see 
                #which threshold to use to say presence of absence
                thresHoldAllIn = min(predsFit)
                threshold095 = quantile(predsFit,probs=0.05)
                
                #Model validation ====
                #target at background and testing points
                ###get coordinates of points in center of each cell in raster
                bkgClim =as.data.frame (clim[[1]],cells=T,na.rm=T)
                if(sum(studyarea$SHAPE_AREA.1)>10) {
                ids = sample (1:nrow (bkgClim),size = 10000 ,replace = F) ## resample centroid 
                }else{ids = sample (1:nrow (bkgClim),size = 1000 ,replace = F)
                }
                leng= length(ids)
                
                #check if 10000 background points are ok 
                if (leng < 2 * nrow(occClimTest) ) {
                  ids = sample (1:nrow (bkgClim),size = 0.1 * nrow (bkgClim),replace = F)
                  #if not ,take 10% of centriod points
                }
                
                #from ids to get cell id
                idsBkg = bkgClim [ids,'cell']
                coo <- terra::xyFromCell(clim[[1]],idsBkg)
                climBkg= cbind(idsBkg,coo) ## get x and y coordinates
                climBkg <- na.omit(climBkg)
                climBkg <- climBkg[!climBkg[,"x"] %in% occ$decimalLongitude |  ###in case of 
                                     !climBkg [,"y"]%in% occ$decimalLatitude,] #overlapping
                
                #model projection ====
                #project to the raster with probability of presence
                clim <- raster::stack(clim) #in order to ovid R session abortion
                outputProb = projectRangeBag(env1 = clim,rbm = rbOcc)
                #plot (outputProb)
                names(outputProb) <- "probalility"
                #convert to binary with threshold 0.95
                outputBin = outputProb>=threshold095
                #plot (outputBin)
                
                #follow climBkg(take coordinates from background step) 
                predsBkg = terra::extract(outputProb,climBkg[,c(2,3)])
                ##probalility of background points
                
                ###summary of testing and background points
                dfEvaluation = data.frame (obs=c(rep(1,times=length (predsFit)), rep (0, length(predsBkg))),
                                           preds= c(predsFit,predsBkg))
                
                #calculate boyce index
                BI <- ecospat::ecospat.boyce(fit=c(predsFit,predsBkg),
                                             obs= predsFit,PEplot=T)
                
                #or continuous boyce index
                CBI1 <- enmSdm::contBoyce(pres=predsFit,
                                          bg=predsBkg,graph=T)
                CBI2<- enmSdm::contBoyce2x(pres=predsFit,
                                           bg=predsBkg,graph=T)
                
                ##detailed explanation for CBI: 
                #https://stackoverflow.com/questions/64223226/continuous-boyce-index-cbi-in-r
                #https://groups.google.com/g/maxent/c/eJVrQOIfOkI
                #https://groups.google.com/g/maxent/c/raPasq0vy2s 
                
                #auc
                aucOut =SDMTools::auc(obs=dfEvaluation$obs,pred = dfEvaluation$preds)
                
                #df with statistics of above index and threshold info
                dfOut <- data.frame(AUC=aucOut,BI=BI$cor,CBI1,CBI2,spName,thresHoldAllIn,threshold095)
                
                
                
                #output 
                outputSpDir = paste0('D:/quercus/Partage/Pengcheng/R/SDM/potential_geographical_range/4/',spName)
                dir.create(outputSpDir, recursive = T)
                writeRaster(x = outputProb,
                            file = paste0(outputSpDir,'/',spName,'_pro.tif'),overwrite=TRUE)
                writeRaster(x = outputBin,
                            file = paste0(outputSpDir,'/',spName,'_bina.tif'),overwrite=TRUE)
                saveRDS (occClimCalibrate,file =paste0(outputSpDir,'/',spName,'_Coo_Calibrate.rds') )
                write.table(x = occClimCalibrate,
                            file = paste0(outputSpDir,'/',spName,'_Calibrate.txt'))
                saveRDS (occClimTest,file =paste0(outputSpDir,'/',spName,'_Coo_Tes.rds') )
                write.table (x = occClimTest,
                             file = paste0(outputSpDir,'/',spName,'_occClimTest.txt'))
                write_csv (x = dfOut,
                           file = paste0(outputSpDir,'/',spName,'_sta.csv'))
                write_rds (dfOut,paste0(outputSpDir,'/',spName,'stats.rds'))
                
                ##remove important something out of list to save RAM 
                rm(list = c('clim','occ','occ_sf',"studyarea" ,
                            "threshold095","thresHoldAllIn","outputProb" ,"outputBin",
                            "rbOcc","occClim","bkgClim","dfEvaluation","climBkg"))
                #clean RAM
                gc()
                tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE) #remove temporary file
                
              } else {print(spName)}
            } else {print(spName)}
          }

# Stop the cluster
stopCluster(cl)
closeAllConnections()
registerDoSEQ()    ###do the sequential coding

View(l)  ###check the results if there is any errors



