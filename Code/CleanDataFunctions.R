difference <- function(df, colName){
  diff <- c(0)
  for(i in 2:dim(df)[1]){
    diff <- diff %>% append(df[colName][[1]][1] - df[colName][[1]][i - 1])
  }
  return(diff)
}


clean_data <- function(df, crop){
  df.crop <- df %>% filter(SC_Commodity_Desc == crop)# %>% filter(SC_Frequency_Desc == "Annual")
  df.filter <- df.crop %>% select(SC_Attribute_Desc, Year_ID, SC_Frequency_Desc, Amount)
  df.group <- df.filter %>% group_by(Year_ID, SC_Frequency_Desc, SC_Attribute_Desc) %>% summarise(sum=sum(Amount)) %>%
    ungroup()
  df.spread <- spread(df.group, SC_Attribute_Desc, sum)
  df.final <- df.spread 
  return(df.final)
}
