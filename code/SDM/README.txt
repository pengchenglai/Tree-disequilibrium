1.To facilitate the spatial operations(join and intersection) as well as probability of occurence simulation which based upon input of 
climatic values(raster layers), we seperate the occurence records into large group(more than 1000) and small group(less or equal to 1000). 
Besides that, we also assign study area(sum of ecoregions) into large area(more than 500) and small area(less or equal to 500), 
which resulting to these four scripts(e.g. raster_terra_large_oc_large_eco)

2.Deprecated_raster_only_small_oc_small_eco is deprecated code that we want to use raster package only in the loop, but find it takes much
more time than using combaination of raster and terra package(it works in the loop)

3.auc_redo is the process of repeating SDM for species whose AUC is lower than 0.8(possible for CBI)


See details in each code


