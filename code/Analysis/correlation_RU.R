##set up library
packs <- list("tidyverse","Hmisc",
              "corrplot")
lapply(packs, require, character.only = T)
#set up working directory 
setwd("D:/quercus/Partage/Pengcheng/R/analyse/code")
# mean values of explanatory variables
mean_df_ana2 <- readRDS("mean_df_ana2.rds")
head(mean_df_ana2)
setwd("D:/quercus/Partage/Pengcheng/R/analyse/annex")
#change the column name
colnames(mean_df_ana2)[c(4,5)] <- c("RU(conservative)","RU(alpha-hull)")

# Step 1: Call the pdf command to start the plot
png(file = "correlation1.png", 
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 2,
  cex.lab  = 2
) # The directory you want to save the file in

# Step 2:
###correlation analysis between HM and tree cover
res <- rcorr(as.matrix(mean_df_ana2[,c(4,5)]),type ="spearman")
res
# Insignificant correlations are leaved blank
corrplot(res$r, type="upper", order="original", 
         p.mat = res$P, sig.level = 0.05, insig = "blank",addCoef.col = "black",
         number.cex = 2,tl.col="black",tl.srt=90,tl.cex=1.5,cl.cex = 0.9)

# Step 3: Run dev.off() to create the file!
dev.off()

