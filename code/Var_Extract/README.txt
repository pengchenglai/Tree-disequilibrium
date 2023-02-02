1. RU is the R script to calculate the range unfilling, of which it produces a statistic map(based on the species' absence/presence 
originated from geographical range and potential geographical range) and its statistical summary(0 means species is abesent according
to both models, 1 represents species only prediceted as present by alpha-hull Algrithm, 11 means species is present based on both models);
This script also generates the mean values of environmental variables across the potential range of the species. 

2. add_centriod helps to add coodinates of the centriod and realm for each species.

3. Each file in staCen(.csv or .rds, suffix) in the subfolder of RU_exVar or RU_exVar_2 is the final data which stores RU, model 
information as well as mean and median values of enviromental variables.