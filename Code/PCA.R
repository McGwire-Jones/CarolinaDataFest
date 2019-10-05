library("FactoMineR")
library("ggplot2")
library("factoextra")
library("corrplot")
library("dplyr")
library("tidyr")
rm(list=ls())

df <- read.csv("https://query.data.world/s/rzinddjz62mmzk63ncdqbt5zfxhbsj", header=TRUE, stringsAsFactors=FALSE)
setwd("~/Documents/CarolinaDataFest/")


for(group in unique_groups[1]){
  df.temp <- df %>% filter(df_temp$SC_Group_Desc == group)
  df.temp.spread <- df.temp %>% spread(SC_Attribute_Desc, value=Amount)
  pca <- PCA()
}


unique_groups <- unique(df$SC_Group_Desc)
df.temp <- df %>% filter(df$SC_Group_Desc == unique_groups[4])
df.temp.spread <- df.temp %>% spread(SC_Attribute_Desc, value=Amount)
temp.pca <- PCA(df.temp.spread[19:length(colnames(df.temp.spread))], graph=FALSE)

fviz_eig(temp.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(temp.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

