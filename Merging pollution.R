######Pollution####
Pollution_Region_1 <- AQI.merged%>%
  filter(AQI.merged$State.Code %in% c("6", "17", "19", "27", "28", "30", "36", "37"))

Pollution_Region_2 <- AQI.merged%>%
  filter(AQI.merged$State.Code %in% c("11", "12", "13", "20", "21", "23", "25", "32", "33", "39", "47"))

Pollution_Region_3 <- AQI.merged%>%
  filter(AQI.merged$State.Code %in% c("1", "8", "9", "16", "22", "34", "40", "41", "44", "46"))

Pollution_Region_4 <- AQI.merged%>%
  filter(AQI.merged$State.Code %in% c("10", "24", "26", "29", "35", "42", "45", "48"))


#Put the pollution data for each of the regions into a list
Pollution.group <- list(Pollution_Region_1, Pollution_Region_2, Pollution_Region_3, Pollution_Region_4)

#Add ADM_MON and SVYEAR columns
for(i in 1:4){Pollution.group[[i]]$ADM_MON <- substr(Pollution.group[[i]]$Date,6,7)
Pollution.group[[i]]$SVYEAR <- substr(Pollution.group[[i]]$Date,1,4)}

#Grouping by month, and applying the mean function to the AQI
mean.AQI <- list()
for(i in 1:4){mean.AQI[[i]] <- aggregate(AQI ~ ADM_MON + SVYEAR, data= Pollution.group[[1]], FUN= mean)
mean.AQI[[i]]$REGION <- rep(i,length(mean.AQI))}



##turn aqi into a list
mean.AQI <- bind_rows(mean.AQI)
mean.AQI$ADM_MON <- as.numeric(mean.AQI$ADM_MON)

#Join with the rest of the data
data2 <- merge(x= data2, y= mean.AQI, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x = TRUE)

