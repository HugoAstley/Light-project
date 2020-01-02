#scale data
data2.short.scaled <- data2.short%>%
  dplyr::select(AGE, AQI)%>%
  scale()


data2.short.scaled <- data2.short
data2.short.scaled$AGE <- scale(data2.short.scaled$AGE)