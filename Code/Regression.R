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
wheat <- clean_data(df, "Wheat")

##### FUNCTIONS ##### 
regression <- function(data, dependent, independent){
  data <- data %>% select(dependent, independent)
  data <- drop_na(data)
  independents <- paste0("`", independent, "`", collapse = "+")
  form <- paste0("`", dependent, "`~", independents, collapse = "")
  print(form)
  model <- lm(as.formula(form), data=data)
  return(model)
}

pca.func <- function(data, vars){
  data <- data %>% select(vars)
  data <- drop_na(data)
  return(PCA(data, graph=FALSE))
}

##### OATS #####
oats.uses <- c("Feed and residual use", "Food, alcohol, and industrial use", "Prices received by farmers")
oats.model <- regression(oats, "Planted acreage", oats.uses)
ggplot(corn, aes(`Feed and residual use`, `Planted acreage`)) + geom_point() + geom_smooth(method="lm")
oats.pca.all <- pca.func(corn, oats.uses)
oats.var <- get_pca_var(oats.pca.all)
fviz_eig(oats.pca.all, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(oats.pca.all, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) 
corrplot(oats.var$contrib, is.corr = FALSE)


#### CORN #####
ggplot(corn, aes(`Planted acreage`), group_by(df$SC_GeographyIndented_Desc)) + geom_histogram(aes(col=df$SC_GeographyIndented_Desc))
ggplot(corn, aes(Year_ID, `Planted acreage`)) + geom_point()
ggplot(corn, aes(Year_ID, `High-fructose corn syrup (HFCS) use`)) + geom_point() + geom_smooth(method="lm")
ggplot(corn, aes(`High-fructose corn syrup (HFCS) use`, `Planted acreage`)) + geom_point()
ggplot(corn, aes(`Feed use`, `Planted acreage`)) + geom_point()
ggplot(corn, aes(Year_ID, Production)) + geom_point()
ggplot(drop_na(corn %>% select(Year_ID, `Alcohol for fuel use`)), aes(Year_ID, `Alcohol for fuel use`)) + geom_point() +  ggtitle("Feed and Residual use vs Year") + xlab("Year")
ggplot(drop_na(corn %>% select(Year_ID, `Alcohol for beverages and manufacturing use`)), aes(Year_ID, `Alcohol for beverages and manufacturing use`)) + geom_point() + ggtitle("Corn Imported vs Year") + xlab("Year")
ggplot(drop_na(corn %>% select(Year_ID, `Feed and residual use`)), aes(Year_ID, `Feed and residual use`)) + geom_point() + ggtitle("Feed and Residual use vs Year") + xlab("Year")
ggplot(drop_na(corn %>% select(Year_ID, `Production`)) %>% filter(Year_ID > 1980), aes(Year_ID, `Production`)) + geom_point() + ggtitle("Corn Production vs Year") + xlab("Year")
ggplot(drop_na(corn %>% select(Year_ID, `Total domestic use`)), aes(Year_ID, `Total domestic use`)) + geom_point() + ggtitle("Corn Domestic Use vs Year") + xlab("Year")
ggplot(drop_na(corn %>% select(Year_ID, `Imports, market year`)), aes(Year_ID, `Imports, market year`)) + geom_point() + ggtitle("Corn Imported vs Year") + xlab("Year")
ggplot(drop_na(corn %>% select(Year_ID, `Exports, market year`)), aes(Year_ID, `Exports, market year`)) + geom_point() + ggtitle("Corn Exported vs Year") + xlab("Year")

corn.uses <- c("Alcohol for fuel use", "Feed use", "High-fructose corn syrup (HFCS) use",
               "Food, seed, and industrial use", "Alcohol for beverages and manufacturing use")
corn.model <- regression(corn, "Production", corn.uses)
corn.slm <- regression(corn, "Production", c("High-fructose corn syrup (HFCS) use"))
corn.pca <- pca.func(corn, corn.uses)
corn.var <- get_pca_var(corn.pca)
fviz_eig(corn.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(corn.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) 
corrplot(corn.var$contrib, is.corr = FALSE)
