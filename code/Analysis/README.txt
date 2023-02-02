This folder contains the scripts for analyzing the dataset and final dataset for 19,041 species which stores in final_analysis(.csv).

1. analyse_data.R
(NOTE: the RU in this script is calcualted at the resolution of 30 arcsec, to analyze RU and environmental variable at 9.8* 9.8 km,
simply set wd into "D:/quercus/Partage/Pengcheng/R/variable_extraction/RU_exVar_2" in the begaining of the script) 
this script first compiles the individual dataset for each species(outputs are mean_df_ana2 & median_df_ana2 for mean and median
value of the environmental data, stores in the dataset_RU); 
then is the step of correlation analysis between explainable variables(first median value,then mean value) at both global and realm scale;
then is the distribution relations between environmental variables and log(RU) of which RU follows lognormal distribution;
final part is regarding glmm and glm: rescale some variables into the same scale as others, and fit glmm for adjusted tree cover and log(RU),
excluding some extreme values based upon cook distance(one variable was excluded based on vif) and get final dataset(stores in the 
dataset_RU) which later on used to build glmm between tree cover& hm and log(RU) since it is not computable directly using mean_df_ana2 
dataset. Last steps were required to build glm for species in each realm(model.a for log(RU)~ ajusted tree cover + climate; model.b for 
log(RU)~ tree cover,human modification & climate),of which vairables were excluded from glm based on vif.
SEE FURTHER DETAILS IN THE SCRIPT

2. correlation_RU.R
this script is used to analyze the correlation relation between RU(conservative) which only consider realized range where overlapped with 
potential range when caculating realized range/potential range ratio and RU(alpha-hull) that taking all the realized range.

3. inital_records.R
this script is used to calculate how many records were left after data cleaning(also exclude duplicate records) and alpha degree derived
from alpha hull method for each species on average

4. random_forest.R
this script is used to analyze relative importance of variables(mean values) for log(RU) 

5.records_in_each_step.R
this script is used to define how many species does not pass the tests or left in each step(11 in total)

6. richness_map.R
this script combined with RU_pattern_thesis.R(2.potential species richness map) is used to calculate global species filling map(summary of
conservative realized range/summary of potential range,in other words filling_onlyin/sdm)

7. RU_pattern_thesis.R
this script is used to produce Figure 1 occurence map for abies alba, Figure 3(RU status quo at global and realm scale),
Figure 4(global species filling map) & Annex Figure 1(RU showed by centriod of species) in the master thesis.

8. sdm_sample.R
this script is used to produce Figure 2(visulization for the calculation of RU) in the master thesis


