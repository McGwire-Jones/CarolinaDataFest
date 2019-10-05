library("dplyr")
library("ggplot2")
rm(list=ls())
df <- read.csv("https://query.data.world/s/rzinddjz62mmzk63ncdqbt5zfxhbsj", header=TRUE, stringsAsFactors=FALSE)
setwd("~/Documents/CarolinaDataFest/")

##### Unfiltered #####
summary(df)
colnames(df)
unique(df$SC_Group_Desc)
unique(df$SC_Unit_Desc)
unique(df$SC_Attribute_Desc)
unique(df$SC_Commodity_Desc)
df.tonsPerHectare <- df %>% filter(SC_Unit_Desc == "Metric tons per hectare")
ggplot(df.tonsPerHectare, aes(Year_ID, Amount), group_by(SC_Commodity_Desc)) + geom_point(aes(col = SC_Commodity_Desc))

df.dollarsPerTon <- df %>% filter(SC_Unit_Desc == "Dollars per ton")
ggplot(df.dollarsPerTon, aes(Year_ID, Amount), group_by(SC_Commodity_Desc)) + geom_point(aes(col = SC_Commodity_Desc))

data <- df
for(i in 1:unique(df$SC_Group_Desc)){
  data$unique(df$SC_Group_Desc)[i] <- data$Amount[data$SC_Group_Desc == unique(data$SC_Group_Desc)[i]]
}


df.fuel <- df %>% filter(SC_Attribute_Desc == "Alcohol for fuel use")
ggplot(df.fuel, aes(Year_ID, Amount)) + geom_point(aes(col = SC_Commodity_Desc))

##### Filtered #####
df.filtered = read.csv("Data/Feed_Grains.csv")

df.filtered$Difference <- 0
for(i in 2:dim(df.filtered)[1]){
  df.filtered$Difference[i] <- df.filtered$Amount[i] - df.filtered$Amount[i - 1]
}
ggplot(df.filtered, aes(Year_ID, Difference)) + geom_point(aes(col = SC_Unit_Desc))



#### Normalizing ####
normalized <- df
normalized$Amount <- scale(normalized$Amount)
mean(normalized$Amount)

#### Time Plots #####
unique_att <- unique(df$SC_Group_Desc)
for(att in unique_att){
  temp_df <- df %>% filter(SC_Group_Desc == att)
  print(
    ggplot(temp_df, aes(Year_ID, Amount), group_by(SC_Attribute_Desc)) + 
      geom_point(aes(col = SC_Attribute_Desc)) + ggtitle(att))
}
