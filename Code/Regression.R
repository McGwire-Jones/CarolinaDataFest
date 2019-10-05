#Change in acreage = Y
#Change in price, change in exports, change in stocks, unharvested
rm(list=ls())
library("dplyr")
library("ggplot2")
library("FactoMineR")
library("ggplot2")
library("factoextra")
library("corrplot")
library("tidyr")


df <- read.csv("https://query.data.world/s/rzinddjz62mmzk63ncdqbt5zfxhbsj", header=TRUE, stringsAsFactors=FALSE)
setwd("~/Documents/CarolinaDataFest/")
source("Code/CleanDataFunctions.R")

oats <- clean_data(df, "Oats")
sorghum <- clean_data(df, "Sorghum")
barley <- clean_data(df, "Barley")
corn <- clean_data(df, "Corn")

regression <- function(data, dependent, independent){
  data <- data %>% select(dependent, independent)
  data <- drop_na(data)
  independents <- paste0("`", independent, "`", collapse = "+")
  form <- paste0("`", dependent, "`~", independents, collapse = "")
  print(form)
  model <- lm(as.formula(form), data=data)
  return(model)
}

test <- regression(oats, "Planted acreage", c("Imports, market year", "Exports, market year"))

oats.pca.all <- PCA(oats[2:dim(oats)[2]], graph=FALSE)
oats.var <- get_pca_var(oats.pca.all)
fviz_eig(oats.pca.all, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(oats.pca.all, alpha.var = "cos2")
fviz_pca_var(oats.pca.all, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) 
corrplot(oats.var$contrib, is.corr = FALSE)
